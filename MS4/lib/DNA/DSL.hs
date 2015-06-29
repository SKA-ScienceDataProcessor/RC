{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , TreeCollector(..)
    , treeCollector
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
      -- ** Actor spawning
    , eval
    , evalClosure
    , availableNodes
    , startActor
    , startCollector
    , startGroup
    -- , startGroupN
    , startCollectorGroup
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

    , waitForResoures
    ) where

import Control.Applicative
import Control.Monad.Operational
import Control.Monad.IO.Class
import Control.Monad.Writer.Strict
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)

import DNA.Types
import DNA.Logging (ProfileHint(..))
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
-- monad. The 'MonadIO' instance is also our only way to execute
-- arbitrary 'IO' actions.
newtype Kern a = Kern { runKern :: IO a }
               deriving (Functor,Applicative,Monad)

data KernelMode
    = DefaultKernel -- ^ No restrictions on kernel execution
    | BoundKernel   -- ^ Kernel relies on being bound to a single OS thread.

instance MonadIO Kern where
    liftIO = Kern

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
    SpawnCollectorGroup
      :: (Serializable a, Serializable b)
      => Res
      -> ResGroup
      -> Spawn (Closure (CollectActor a b))
      -> DnaF (Shell (Grp a) (Grp b))
    SpawnCollectorTreeGroup
      :: (Serializable a)
      => Res
      -> Spawn (Closure (TreeCollector a))
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
      -> (b -> a -> IO b)
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

-- | Monad for spawning new actor processes
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

-- | Spawn the process on the local node
useLocal :: Spawn ()
useLocal = Spawn $ tell [UseLocal]

-- | Spawn the process using the "failout" model.
failout :: Spawn ()
failout = Spawn $ tell [UseFailout]

timeout :: Double -> Spawn ()
timeout t = Spawn $ tell [UseTimeout t]

-- | Marks the processes to respawn after fail
respawnOnFail :: Spawn ()
respawnOnFail = Spawn $ tell [UseRespawn]

-- | Modify actor spawn for debugging
debugFlags :: [DebugFlag] -> Spawn ()
debugFlags fs = Spawn $ tell [UseDebug fs]

-- | Result of an actor's computation, which might not yet be
-- received.
newtype Promise a = Promise (ReceivePort a)

-- | Like 'Promise', but stands for the a group of results, as
-- generated by an actor group.
data Group a = Group (ReceivePort a) (ReceivePort Int)

----------------------------------------------------------------
-- Data types for actors
----------------------------------------------------------------

-- | This is the simplest kind of actor. It receives exactly one
-- message of type @a@ and produce a result of type @b@.
data Actor a b where
    Actor :: (Serializable a, Serializable b) => (a -> DNA b) -> Actor a b
    deriving (Typeable)

-- | Smart constructor for 'Actor's. As the type signature shows, an
-- `Actor` is constructed from a function that takes a parameter `a`
-- and returns a result `b`. The `DNA` monad allows the actor to take
-- further actions, such as spawning other actors or starting data
-- transfers.
actor :: (Serializable a, Serializable b)
      => (a -> DNA b) -- ^ data flow definition
      -> Actor a b
actor = Actor

-- | In contrast to a simple `Actor`, actors of this type can receive
--   a group of messages. However, it will still produce just a
--   singular message. In functional programming terms, this actor
--   corresponds to a 'fold', which reduces an unordered set of
--   messages into an aggregate output value.
data CollectActor a b where
    CollectActor :: (Serializable a, Serializable b)
                 => (s -> a -> IO s)
                 -> IO s
                 -> (s -> IO b)
                 -> CollectActor a b
    deriving (Typeable)

-- | Just like a 'fold', a 'CollectorActor' is defined in terms of an
-- internal state which gets updated for every message received. To be
-- precise, the state first gets initialised using a start value, then
-- gets updated successively using the stepper function. Once all
-- results have been received, the termination function generates the
-- overall result value of the actor.
collectActor
    :: (Serializable a, Serializable b, Serializable s)
    => (s -> a -> IO s) -- ^ stepper function
    -> IO s             -- ^ start value
    -> (s -> IO b)      -- ^ termination function
    -> CollectActor a b
collectActor = CollectActor

-- | Collector which could collect data in tree-like fashion
data TreeCollector a where
    TreeCollector :: (Serializable a)
                  => (a -> a -> IO a)
                  -> IO a   
                  -> TreeCollector a
    deriving (Typeable)

-- | Smart constructor for tree collector.
treeCollector :: Serializable a => (a -> a -> IO a) -> IO a -> TreeCollector a
treeCollector = TreeCollector



----------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------

-- | Obtains the rank of the current process in its group. Every
-- process in a group of size /N/ has assigned a rank from /0/ to
-- /N-1/. Single processes always have rank /0/.
rank :: DNA Int
rank = DNA $ singleton DnaRank

-- | Obtains the size of the group that the current process belongs
-- to. For single processes this is always /1/.
groupSize :: DNA Int
groupSize = DNA $ singleton DnaGroupSize

-- | Executes a kernel computation. A bound thread will be
-- allocated in order to perform the computation.
kernel :: String        -- ^ Kernel name
       -> [ProfileHint] -- ^ Kernel performance characteristics
       -> Kern a        -- ^ Kernel code
       -> DNA a
kernel msg hints = DNA . singleton . Kernel msg BoundKernel hints

-- | A variant of 'kernel' that executes the kernel in an /unbound/
-- thread. This is generally faster, but less safe. Especially
-- profiling can be unreliable in this mode.
unboundKernel :: String -> [ProfileHint] -> Kern a -> DNA a
unboundKernel msg hints = DNA . singleton . Kernel msg DefaultKernel hints

-- | Basic profiling for 'DNA' actions.
duration :: String -> DNA a -> DNA a
duration msg = DNA . singleton . Duration msg

-- | Outputs a message to the eventlog as well as 'stdout'. Useful for
-- documenting progress.
logMessage :: String -> DNA ()
logMessage = DNA . singleton . LogMessage

-- | Obtains a promise from a shell. This amounts to connecting the
-- actor.
delay :: Serializable b => Location -> Shell a (Val b) -> DNA (Promise b)
delay loc sh = DNA $ singleton $ Delay loc sh

-- | Can be used to obtain the result from a 'Promise'. If the actor
-- didn't sent result yet, it will block until the value arrives.
await :: Serializable a => Promise a -> DNA a
await = DNA . singleton . Await

-- | Like 'delay', but for a 'Grp' of actors. Consequently, we produce
-- a promise 'Group'.
delayGroup :: Serializable b => Shell a (Grp b) -> DNA (Group b)
delayGroup = DNA . singleton . DelayGroup

-- | Obtains results from a group of actors by folding over the
-- results.
gather
    :: Serializable a
    => Group a
    -> (b -> a -> b)
    -> b
    -> DNA b
gather g f = gatherM g (\b a -> return $ f b a)

-- | Like 'gather', but allows us to execute an 'IO' action for every
-- chunk of data we receive.
gatherM
    :: Serializable a
    => Group a
    -> (b -> a -> IO b)
    -> b
    -> DNA b
gatherM g f b = DNA $ singleton $ GatherM g f b

-- | Send a value to an actor.
sendParam :: Serializable a => a -> Shell (Val a) b -> DNA ()
sendParam a sh = DNA $ singleton $ SendParam a sh

-- | Broadcast a value to a group of actors. The 'Scatter' explains
-- how the data is meant to get split up.
broadcast :: Serializable a => a -> Shell (Scatter a) b -> DNA ()
broadcast a sh = DNA $ singleton $ Broadcast a sh

-- | Distribute work between group of actors.
distributeWork
    :: Serializable b
    => a -- ^ Parameter we want to send
    -> (Int -> a -> [b])
    -- ^ Function which distribute work between actors. First
    -- parameter is length of list to produce
    -> Shell (Scatter b) c
    -> DNA ()
distributeWork a f sh = DNA $ singleton $ DistributeWork a f sh

-- | Only function for connecting the output of one actor to the input of
-- another one. Only actors with matching input and output types can
-- be connected.
connect :: (Serializable b, Typeable tag)
        => Shell a (tag b) -> Shell (tag b) c -> DNA ()
connect a b = DNA $ singleton $ Connect a b

-- | Returns the number of nodes that are available for spawning
-- remote processes on.
availableNodes :: DNA Int
availableNodes = DNA $ singleton AvailNodes

-- | The simplest form of actor execution: The actor is executed in
-- the same Cloud Haskell process, with no new processes spawned.
eval :: (Serializable a, Serializable b)
     => Actor a b
     -> a
     -> DNA b
eval (Actor act) = act

-- | Like 'eval', but uses a 'Closure' of the actor code.
evalClosure :: (Typeable a, Typeable b)
            => Closure (Actor a b)
            -> a
            -> DNA b
evalClosure clos a = DNA $ singleton $ EvalClosure a clos

-- | Starts a single actor as a new process, and returns the handle to
-- the running actor.
startActor
    :: (Serializable a, Serializable b)
    => Res
    -> Spawn (Closure (Actor a b))
    -> DNA (Shell (Val a) (Val b))
startActor r a =
    DNA $ singleton $ SpawnActor r a

-- | As 'startActor', but starts a single collector actor.
startCollector :: (Serializable a, Serializable b)
               => Res
               -> Spawn (Closure (CollectActor a b))
               -> DNA (Shell (Grp a) (Val b))
startCollector res child =
    DNA $ singleton $ SpawnCollector res child

-- | Start a group of actor processes
startGroup :: (Serializable a, Serializable b)
           => Res
           -> ResGroup
           -> Spawn (Closure (Actor a b))
           -> DNA (Shell (Scatter a) (Grp b))
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

-- | Start a group of collector actor processes
startCollectorGroup
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (CollectActor a b))
    -> DNA (Shell (Grp a) (Grp b))
startCollectorGroup res resG child =
    DNA $ singleton $ SpawnCollectorGroup res resG child

-- | Start a group of collector actor processes
startCollectorTreeGroup
    :: (Serializable a)
    => Res
    -> Spawn (Closure (TreeCollector a))
    -> DNA (Shell (Grp a) (Grp a))
startCollectorTreeGroup res child =
    DNA $ singleton $ SpawnCollectorTreeGroup res child

crashMaybe :: Double -> DNA ()
crashMaybe = DNA . singleton . CrashMaybe

-- | Allocates a new file channel for sharing data between actors.
createFileChan :: Location -> String -> DNA (FileChan a)
createFileChan loc name = DNA $ singleton $ CreateFileChan loc name

-- | Barrier that ensures that all resources associated with the given
-- shell have been returned and can be re-allocated.
waitForResoures :: Shell a b -> DNA ()
waitForResoures (Shell aid) = DNA $ singleton $ WaitForResources aid
