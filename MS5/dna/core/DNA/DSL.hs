{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
-- | Description of DNA DSL as operational monad
module DNA.DSL (
      -- * Base DSL
      DNA(..)
    , DnaF(..)
    , Promise(..)
    , Group(..)
    , Kern(..)
      -- ** Spawn monad
    , Spawn(..)
    , SpawnFlag(..)
    , DebugFlag(..)
    , runSpawn
    , useLocal
    , failout
    , timeout
    , respawnOnFail
    , debugFlags
      -- * Actors
    , Actor(..)
    , actor
    , CollectActor(..)
    , collectActor
      -- * Smart constructors
      -- ** Logging
    , logMessage
    , duration
    , ProfileHint(..)
      -- ** Other
    , Rank
    , rank
    , groupSize
    , kernel
    , unboundKernel
    , KernelMode(..)
    , mkKernRunner
      -- ** Actor spawning
    , eval
    , evalClosure
    , availableNodes
    , startActor
    , startCollector
    , startGroup
    -- , startGroupN
    , startCollectorTree
    , startCollectorTreeGroup
      -- ** Dataflow building
    , delay
    , await
    , delayGroup
    , gather
    , gatherM
    , sendParam
    , broadcast
    , distributeWork
    , connect

    , crashMaybe
    , FileChan
    , createFileChan

    , waitForResources
    ) where

import Control.Monad.Operational
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)

import DNA.Types
import DNA.Logging (MonadLog(..), LoggerOpt, ProfileHint(..))
import DNA.Channel.File (FileChan)

----------------------------------------------------------------
-- Operations data type for DNA
----------------------------------------------------------------

-- | Monad for defining the behaviour of a cluster application. This
-- concerns resource allocations as well as steering data and control
-- flow.
newtype DNA a = DNA (Program DnaF a)
                deriving (Functor,Applicative,Monad)

-- | Monad for actual calculation code. We expect all significant
-- work of the cluster application to be encapsulated in this
-- monad. In fact, the only way to perform arbitrary 'IO' actions from
-- 'DNA' is to use 'kernel' or 'unboundKernel' and then 'liftIO' the
-- desired code:
--
-- >   kernel "do IO" $ liftIO $ do
-- >     someIoComputation
--
-- Pure computations should be lifted into the 'Kern' monad as well
-- whenever they are likely to require a significant amount of
-- computation. However care needs to be taken that no thunks escape
-- due to lazy evaluation. Ideally, the result should be fully
-- evaluated:
--
-- >   kernel "pure computation" $ do
-- >     let pure = pureCode
-- >     return $! pure `using` rdeepseq
newtype Kern a = Kern { runKern :: ReaderT (String, LoggerOpt) IO a }
               deriving (Functor,Applicative,Monad)

data KernelMode
    = DefaultKernel -- ^ No restrictions on kernel execution
    | BoundKernel   -- ^ Kernel relies on being bound to a single OS thread.

instance MonadIO Kern where
    liftIO = Kern . lift
instance MonadLog Kern where
    logSource = Kern (fst <$> ask)
    logLoggerOpt = Kern (snd <$> ask)

mkKernRunner :: (MonadLog m, MonadIO m2) => m (Kern a -> m2 a)
mkKernRunner = do
  logSrc <- logSource
  logOpt <- logLoggerOpt
  return (liftIO . flip runReaderT (logSrc, logOpt) . runKern)

-- | GADT which describe operations supported by DNA DSL
data DnaF a where
    -- Execute foreign kernel
    Kernel
      :: String
      -> KernelMode
      -> [ProfileHint]
      -> Kern a
      -> DnaF a
    DnaRank :: DnaF Int
    DnaGroupSize :: DnaF Int

    AvailNodes :: DnaF Int

    LogMessage :: String -> DnaF ()
    Duration :: String -> DNA a -> DnaF a

    -- Evaluate actor closure
    EvalClosure
      :: (Typeable a, Typeable b)
      => a
      -> Closure (Actor a b)
      -> DnaF b
    -- Spawn single process
    SpawnActor
      :: (Serializable a, Serializable b)
      => Res
      -> Spawn (Closure (Actor a b))
      -> DnaF (Shell (Val a) (Val b))
    SpawnCollector
      :: (Serializable a, Serializable b)
      => Res
      -> Spawn (Closure (CollectActor a b))
      -> DnaF (Shell (Grp a) (Val b))
    SpawnGroup
      :: (Serializable a, Serializable b)
      => Res
      -> ResGroup
      -> Spawn (Closure (Actor a b))
      -> DnaF (Shell (Scatter a) (Grp b))
    -- SpawnGroupN
    --   :: (Serializable a, Serializable b)
    --   => Res
    --   -> ResGroup
    --   -> Int
    --   -> Spawn (Closure (Actor a b))
    --   -> DnaF (Shell (Val a) (Grp b))
    SpawnCollectorTree
      :: (Serializable a)
      => Spawn (Closure (CollectActor a a))
      -> DnaF (Shell (Grp a) (Val a))
    SpawnCollectorTreeGroup
      :: (Serializable a)
      => Res
      -> Spawn (Closure (CollectActor a a))
      -> DnaF (Shell (Grp a) (Grp a))

    -- Connect running actors
    Connect
      :: (Serializable b, Typeable tag)
      => Shell a (tag b)
      -> Shell (tag b) c
      -> DnaF ()
    -- Send parameter to the actor
    SendParam
      :: Serializable a
      => a
      -> Shell (Val a) b
      -> DnaF ()
    -- Send parameter to the actor
    Broadcast
      :: Serializable a
      => a
      -> Shell (Scatter a) b
      -> DnaF ()
    -- Distribute work between set of workers
    DistributeWork
      :: Serializable b
      => a
      -> (Int -> a -> [b])
      -> Shell (Scatter b) c
      -> DnaF ()

    -- Delay actor returning single value
    Delay
      :: Serializable b
      => Location
      -> Shell a (Val b)
      -> DnaF (Promise b)
    DelayGroup
      :: Serializable b
      => Shell a (Grp b)
      -> DnaF (Group b)
    Await
      :: Serializable a
      => Promise a
      -> DnaF a
    GatherM
      :: Serializable a
      => Group a
      -> (b -> a -> Kern b)
      -> b
      -> DnaF b
    CrashMaybe
      :: Double -> DnaF ()

    -- Wait for release of resources by an actor
    WaitForResources :: AID -> DnaF ()

    -- Create a new file channel
    CreateFileChan
      :: Location
      -> String
      -> DnaF (FileChan b)

-- |
-- Monad for accumulating optional parameters for spawning
-- processes. It exists only to (ab)use do-notation and meant to be
-- used as follows:
--
-- > do useLocal
-- >    return $(mkStaticClosure 'actorName)
newtype Spawn a = Spawn (Writer [SpawnFlag] a)
                  deriving (Functor,Applicative,Monad)

-- | Flags for spawn
data SpawnFlag
    = UseLocal
    | UseFailout
    | UseTimeout Double
    | UseRespawn
    | UseDebug [DebugFlag]
    deriving (Show,Eq,Typeable)

-- | Flags which could be passed to actors for debugging purposes
data DebugFlag
    = CrashProbably Double
      -- ^ Crash during startup with given probability. Not all actors
      --   will honor that request
    | EnableDebugPrint Bool
      -- ^ Enable debug printing. If parameter is true child actors
      --   will have debug printing enabled too.
    deriving (Show,Eq,Typeable,Generic)
instance Binary DebugFlag

runSpawn :: Spawn a -> (a,[SpawnFlag])
runSpawn (Spawn m) = runWriter m

-- | With this parameter new actor will be spawned on same node as
-- parent actor. In case of group of actors one of newly spawned
-- actors will run on local node. Otherwise it will be spawned on
-- other node. See documentation for 'Res' for description of
-- interaction of this flag with resource allocation.
useLocal :: Spawn ()
useLocal = Spawn $ tell [UseLocal]

-- | Spawn the process using the "failout" fault-tolerance model. Only
-- valid for group of processes (it's ignored for spawning single
-- process actors). If some actor in group fails group will still
-- continue.
failout :: Spawn ()
failout = Spawn $ tell [UseFailout]

-- | Add timeout for actor. If actor doesn't complete execution in
-- allocated time it will be terminated forcedly. Such termination is
-- treated as crash for purposes of fault tolerance.
timeout :: Double               -- ^ Timeout in seconds
        -> Spawn ()
timeout t = Spawn $ tell [UseTimeout t]

-- | Try to respawn actor in case of crash.
respawnOnFail :: Spawn ()
respawnOnFail = Spawn $ tell [UseRespawn]

-- | Set debugging flags. They are mostly useful for debugging DNA itself.
debugFlags :: [DebugFlag] -> Spawn ()
debugFlags fs = Spawn $ tell [UseDebug fs]

-- | Result of an actor's computation. It could be generated by
-- 'delay' and actual value extracted by 'await'
--
-- > do ...
-- >    p <- delay someActor
-- >    ...
-- >    a <- await p
newtype Promise a = Promise (ReceivePort a)

-- | Obtains a promise from a shell. This amounts to connecting the
-- actor.
delay :: Serializable b
      => Location               -- ^
      -> Shell a (Val b)        -- ^ Actor to obtain promise from.
      -> DNA (Promise b)
delay loc sh = DNA $ singleton $ Delay loc sh

-- | Extract value from 'Promise', will block until value arrives
await :: Serializable a
      => Promise a              -- ^ Promise to extract value from
      -> DNA a
await = DNA . singleton . Await

-- | Like 'Promise', but stands for the a group of results, as
-- generated by an actor group. It could be used in likewise
-- manner. In example below values produced by group of actors @grp@
-- are summed in call to 'gather'.
--
-- > do ...
-- >    p <- delayGroup grp
-- >    ...
-- >    a <- gather p (+) 0
data Group a = Group (ReceivePort a) (ReceivePort Int)

-- | Like 'delay', but for a 'Grp' of actors. Consequently, we produce
-- a promise 'Group'.
delayGroup
    :: Serializable b
    => Shell a (Grp b)          -- ^ Actor to obtain promise from
    -> DNA (Group b)
delayGroup = DNA . singleton . DelayGroup

-- | Obtains results from a group of actors by folding over the
-- results. It behaves like 'CollectActor' but all functions are
-- evaluated locally. It will block until all messages are collected.
gather
    :: Serializable a
    => Group a                  -- ^ Promise to use.
    -> (b -> a -> b)            -- ^ Stepper function (called for each message)
    -> b                        -- ^ Initial value
    -> DNA b
gather g f = gatherM g (\b a -> return $ f b a)

-- | Like 'gather', but allows us to execute an 'IO' action for every
-- chunk of data we receive.
gatherM
    :: Serializable a
    => Group a                  -- ^ Promise to use.
    -> (b -> a -> Kern b)       -- ^ Stepper function (called for each message)
    -> b                        -- ^ Initial value
    -> DNA b
gatherM g f b = DNA $ singleton $ GatherM g f b


----------------------------------------------------------------
-- Data types for actors
----------------------------------------------------------------

-- | This is the simplest kind of actor. It receives exactly one
-- message of type @a@ and produce a result of type @b@. It could only
-- be constructed using 'actor' function.
data Actor a b where
    Actor :: (Serializable a, Serializable b) => (a -> DNA b) -> Actor a b
    deriving (Typeable)

-- | Smart constructor for 'Actor's. As the type signature shows, an
-- `Actor` is constructed from a function that takes a parameter `a`
-- and returns a result `b`. The `DNA` monad allows the actor to take
-- further actions, such as spawning other actors or starting data
-- transfers.
--
-- For example following actor adds one to its parameter
--
-- > succActor :: Actor Int Int
-- > succActor = actor $ \i -> return (i+1)
actor :: (Serializable a, Serializable b)
      => (a -> DNA b) -- ^ data flow definition
      -> Actor a b
actor = Actor

-- | In contrast to a simple `Actor`, actors of this type can receive
--   a group of messages. However, it will still produce just a
--   singular message. In functional programming terms, this actor
--   corresponds to a 'fold', which reduces an unordered set of
--   messages into an aggregate output value. It could only be
--   constructed using 'collectActor' function.
data CollectActor a b where
    CollectActor :: (Serializable a, Serializable b)
                 => (s -> a -> Kern s)
                 -> Kern s
                 -> (s -> Kern b)
                 -> CollectActor a b
    deriving (Typeable)

-- | Just like a 'fold', a 'CollectorActor' is defined in terms of an
-- internal state which gets updated for every message received. To be
-- precise, the state first gets initialised using a start value, then
-- gets updated successively using the stepper function. Once all
-- results have been received, the termination function generates the
-- overall result value of the actor.
--
-- In this example actor sums its parameters. It's very simple
-- actor. In this case type of accumulator (@s@ above) is same as type
-- of resulting value (@Double@) but this isn't necessary. It also
-- doesn't do any IO.
--
-- > sumActor :: CollectorActor Double Double
-- > sumActor = collectActor
-- >     (\sum a -> return (sum + a))
-- >     (return 0)
-- >     (\sum -> return sum)
collectActor
    :: (Serializable a, Serializable b, Serializable s)
    => (s -> a -> Kern s) -- ^ stepper function
    -> Kern s             -- ^ start value
    -> (s -> Kern b)      -- ^ termination function
    -> CollectActor a b
collectActor = CollectActor



----------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------

-- | Obtains the rank of the current process in its group. Every
-- process in a group of size /N/ has assigned a rank from /0/ to
-- /N-1/. Single processes always have rank /0/. It should be used as
-- follows:
--
-- > do ...
-- >    n <- rank
-- >    ...
rank :: DNA Int
rank = DNA $ singleton DnaRank

-- | Obtains the size of the group that the current process belongs
-- to. For single processes this is always /1/. It should be used as
-- follows:
--
-- > do ...
-- >    n <- groupSize
-- >    ...
groupSize :: DNA Int
groupSize = DNA $ singleton DnaGroupSize

-- | Executes a kernel computation. The computation will be bound to
-- an operating system thread by default (see also 'unboundKernel').
-- The function will block until computation is done. Profile hints
-- can be used to request profiling where desired.
--
-- For example, we could define @ddpReadVector@ as used in the DNA
-- example as follows:
--
-- >  ddpReadVector = actor $ \(fname, Slice off n) ->
-- >    kernel "read vector" [iOHint{hintReadBytes = fromIntegral (n * 8)}] $
-- >      liftIO $ readData n off fname
--
-- This "actor" reads a certain slice of a file from the disk, which
-- is implemented using a "kernel" calling the @readData@ 'IO'
-- action. As with most kernels, this could potentially become a
-- bottleneck, therefore we supply DNA with a meaningful name (@read
-- vector@) as well as a hint about how much I/O activity we
-- expect. This will prompt the profiling framework to gather evidence
-- about the actual I/O activity so we can compare it with our
-- expectations.
kernel :: String        -- ^ Kernel name. This name will be used in
                        -- profile analysis to refer to profiling data
                        -- collected about the contained code.
       -> [ProfileHint] -- ^ Kernel performance characteristics. This
                        -- will prompt the framework to track
                        -- specialised performance metrics, allowing
                        -- in-depth analysis later.
       -> Kern a        -- ^ Th kernel code to execute.
       -> DNA a
kernel msg hints = DNA . singleton . Kernel msg BoundKernel hints

-- | A variant of 'kernel' that executes the kernel in an /unbound/
-- thread. Haskell runtime could migrate unbound haskell threads
-- between OS threads. This is generally faster, but less
-- safe. Especially profiling can be unreliable in this mode.
--
-- The most likely use for this is cheap kernels that are unlikely to
-- run for a significant time. For example, we could use an unbound
-- kernel for cleaning up data:
--
-- >    unboundKernel "delete vector" [] $
-- >      liftIO $ removeFile fname
--
-- Here we know that 'removeFile' is safe to be called from unbound
-- kernels, and likely cheap enough that allocating a full operating
-- system thread can be considered overkill.
unboundKernel
    :: String        -- ^ Kernel name
    -> [ProfileHint] -- ^ Kernel performance characteristics
    -> Kern a        -- ^ Kernel code
    -> DNA a
unboundKernel msg hints = DNA . singleton . Kernel msg DefaultKernel hints

-- | Basic profiling for 'DNA' actions. Works basically the same way
-- as 'kernel', but without the specialised profiling
-- support. Instead, the profiling report will only contain the wall
-- clock time the contained 'DNA' action took.
--
-- For example, in the DNA example we used 'duration' to profile how
-- long a 'Promise' was 'await'ed:
--
-- >    va <- duration "receive compute" $ await futVA
--
-- It will result in eventlog output similar to:
--
-- > 941813583: cap 0: START [pid=pid://localhost:40000:0:12] receive compute
-- > ...
-- > 945372376: cap 0: END [pid=pid://localhost:40000:0:12] receive compute
duration
    :: String        -- ^ Computation name for profiling
    -> DNA a         -- ^ 'DNA' code to profile
    -> DNA a
duration msg = DNA . singleton . Duration msg

-- | Outputs a message to the eventlog as well as @stdout@. Useful for
-- documenting progress and providing debugging information.
--
-- For example, we could have an actor log the amount of resource it
-- has available:
--
-- > do avail <- availableNodes
-- >    logMessage $ "Actor is running on " ++ show (avail+1) ++ " nodes."
--
-- It will produce eventlog output similar to this
--
-- > 713150762: cap 0: MSG [pid=pid://localhost:40000:0:10] Actor is running on 8 node
logMessage :: String -> DNA ()
logMessage = DNA . singleton . LogMessage

-- | Send input parameter to an actor. Calling this function twice
-- will result in runtime error. 
--
--  > do ...
--  >    a <- startActor (N 1) (return $(mkStaticClosure 'someActor))
--  >    sendParam 100
--  >    ...
sendParam
    :: Serializable a
    => a                        -- ^ Parameter to send
    -> Shell (Val a) b          -- ^ Actor to send parameter to
    -> DNA ()
sendParam a sh = DNA $ singleton $ SendParam a sh

-- | Send same value to all actors in group. Essentially same as
-- 'sendParam' but works for group of actors.
--
--  > do ...
--  >    a <- startGroup (Frac 0.5) (NNodes 1) (return $(mkStaticClosure 'someActor))
--  >    broadcast 100
--  >    ...
broadcast
    :: Serializable a
    => a                        -- ^ Parameter to send
    -> Shell (Scatter a) b      -- ^ Group of actors to send parameter to
    -> DNA ()
broadcast a sh = DNA $ singleton $ Broadcast a sh

-- | Distribute work between group of actors. @distributeWork a f@
-- will send values produced by function @f@ to each actor in
-- group. Computation is performed locally.
distributeWork
    :: Serializable b
    => a
       -- ^ Parameter we want to send
    -> (Int -> a -> [b])
       -- ^ Function which distribute work between actors. First
       -- parameter is length of list to produce. It must generate
       -- list of required length. 
    -> Shell (Scatter b) c
       -- ^ Group of actors to send parameter to
    -> DNA ()
distributeWork a f sh = DNA $ singleton $ DistributeWork a f sh

-- | Connect output of one actor to input of another actor. In example we connect
-- output of group of actors to collect actor.
--
-- > do ...
-- >    a <- startGroupN (N 10) (NNodes 1) (return $(mkStaticClosure 'worker))
-- >    c <- startCollector (N 1) (return $(mkStaticClosure 'collector))
-- >    connect a c
-- >    ...
connect
    :: (Serializable b, Typeable tag)
    => Shell a (tag b)          -- ^ Actor which produce message(s)
    -> Shell (tag b) c          -- ^ Actor which receives message(s)
    -> DNA ()
connect a b = DNA $ singleton $ Connect a b

-- | Returns the number of nodes that are available at the moment for spawning
-- of remote processes.
availableNodes :: DNA Int
availableNodes = DNA $ singleton AvailNodes

-- | If one don't want to create new actor it's possible to execute
-- simple 'Actor' inside current actor. For example:
-- 
-- > do ...
-- >    b <- eval someActor 42
-- >    ...
eval :: (Serializable a, Serializable b)
     => Actor a b               -- ^ Actor to execute
     -> a                       -- ^ Value which is passed to an actor as parameter
     -> DNA b
eval (Actor act) = act

-- | Like 'eval', but uses a 'Closure' of the actor code.
evalClosure
    :: (Typeable a, Typeable b)
    => Closure (Actor a b)      -- ^ Actor to execute
    -> a                        -- ^ Value which is passed to an actor as parameter
    -> DNA b
evalClosure clos a = DNA $ singleton $ EvalClosure a clos

-- | Starts a single actor as a new process, and returns the handle to
-- the running actor. Spawned actor will receive single message and
-- produce single result as described by 'Val' type tags.
startActor
    :: (Serializable a, Serializable b)
    => Res
       -- ^ How many nodes do we want to allocate for actor
    -> Spawn (Closure (Actor a b))
       -- ^ Actor to spawn
    -> DNA (Shell (Val a) (Val b))
       -- ^ Handle to spawned actor
startActor r a =
    DNA $ singleton $ SpawnActor r a

-- | As 'startActor', but starts collector actor. It receives groups
-- of messages from group of actors and produces single result.
startCollector
    :: (Serializable a, Serializable b)
    => Res
       -- ^ How many nodes do we want to allocate for actor
    -> Spawn (Closure (CollectActor a b))
       -- ^ Actor to spawn
    -> DNA (Shell (Grp a) (Val b))
       -- ^ Handle to spawned actor
startCollector res child =
    DNA $ singleton $ SpawnCollector res child

-- | Start a group of actor processes. They receive set of values
-- which could be sent to them using 'broadcast' or 'distributeWork'
-- and produce group of values as result.
startGroup
    :: (Serializable a, Serializable b)
    => Res
       -- ^ How many nodes do we want to allocate for actor
    -> ResGroup
       -- ^ How to divide nodes between actors in group
    -> Spawn (Closure (Actor a b))
       -- ^ Actor to spawn
    -> DNA (Shell (Scatter a) (Grp b))
       -- ^ Handle to spawned actor
startGroup res resG child =
    DNA $ singleton $ SpawnGroup res resG child

-- -- | Similar to group about, but allows to schedule more actors than
-- -- available resources. Number of actors is given by third
-- -- parameter. Note that the same input will be send to all actors in group
-- -- as evidenced by input tag of Shell.
-- startGroupN
--     :: (Serializable a, Serializable b)
--     => Res         -- ^ Resources for actors
--     -> ResGroup    -- ^
--     -> Int
--     -> Spawn (Closure (Actor a b))
--     -> DNA (Shell (Val a) (Grp b))
-- startGroupN res resG nTasks child =
--     DNA $ singleton $ SpawnGroupN res resG nTasks child

-- | Start a group of collector actor processes. It always require one
-- node.
startCollectorTree
    :: (Serializable a)
    => Spawn (Closure (CollectActor a a))
       -- ^ Actor to spawn
    -> DNA (Shell (Grp a) (Val a))
       -- ^ Handle to spawned actor
startCollectorTree child =
    DNA $ singleton $ SpawnCollectorTree child

-- | Start a group of collector actor processes to collect data in
-- tree-like fashion. They collect data from group of actors and
-- divide it between themselves. So if we have 12 worker actors in a
-- group and 3 actor in group of collectors collector with rank 0 will
-- collect results from workers with rank 0..3 etc. Collectors will
-- produce 3 result which in turn should be aggregated by another
-- collector.
startCollectorTreeGroup
    :: (Serializable a)
    => Res
       -- ^ How many nodes do we want to allocate for group of actors
    -> Spawn (Closure (CollectActor a a))
       -- ^ Actor to spawn
    -> DNA (Shell (Grp a) (Grp a))
       -- ^ Handle to spawned actor
startCollectorTreeGroup res child =
    DNA $ singleton $ SpawnCollectorTreeGroup res child

crashMaybe :: Double -> DNA ()
crashMaybe = DNA . singleton . CrashMaybe

-- | Allocates a new file channel for sharing data between actors.
createFileChan
    :: Location   -- ^ If 'Local' will try to create channel in
                  --   @/ramdisks@ or @/tmp@ if possible.
    -> String     -- ^ Channel name
    -> DNA (FileChan a)
createFileChan loc name = DNA $ singleton $ CreateFileChan loc name

-- | Barrier that ensures that all resources associated with the given
-- actor have been returned to pool and can be re-allocated. It will
-- block until resources are returned. 
--
-- > do a <- startActor ...
-- >    ...
-- >    waitForResources a
--
-- After @waitForResources a@ it's guaranteed that resources allocated
-- to actor @a@ have been returned.
--
-- N.B. It only ensures that actor released resources. They could be
-- taken by another start* function.
waitForResources :: Shell a b -> DNA ()
waitForResources (Shell aid) = DNA $ singleton $ WaitForResources aid
