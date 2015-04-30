{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP                        #-}
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
    , Mapper(..)
    , mapper
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
    , startGroupN
    , startCollectorGroup
    , startCollectorGroupMR
    , startMappers
      -- ** Dataflow building
    , delay
    , await
    , delayGroup
    , gather
    , gatherM
    , sendParam
    -- , broadcastParamSlice
    , broadcast
    , connect
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

----------------------------------------------------------------
-- Operations data type for DNA
----------------------------------------------------------------

newtype DNA a = DNA (Program DnaF a)
                deriving (Functor,Applicative,Monad)

newtype Kern a = Kern { runKern :: IO a }
               deriving (Functor,Applicative,Monad)

data KernelMode
    = DefaultKernel -- ^ No restrictions on kernel execution
    | BoundKernel   -- ^ Kernel relies on being bound to a single OS thread.

instance MonadIO Kern where
    liftIO = Kern

-- Haddock doesn't support GADT constructor docs yet
#ifndef __HADDOCK_VERSION__
#define GADT_HADD(x) x
#else
#define GADT_HADD(x)
#endif

-- | GADT which describe operations supported by DNA DSL
data DnaF a where
    GADT_HADD(-- | Execute foreign kernel)
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

    GADT_HADD(-- | Evaluate actor closure)
    EvalClosure
      :: (Typeable a, Typeable b)
      => a
      -> Closure (Actor a b)
      -> DnaF b
    GADT_HADD(-- | Spawn single process)
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
    SpawnGroupN
      :: (Serializable a, Serializable b)
      => Res
      -> ResGroup
      -> Int
      -> Spawn (Closure (Actor a b))
      -> DnaF (Shell (Val a) (Grp b))
    SpawnCollectorGroup
      :: (Serializable a, Serializable b)
      => Res
      -> ResGroup   
      -> Spawn (Closure (CollectActor a b))
      -> DnaF (Shell (Grp a) (Grp b))
    SpawnCollectorGroupMR
      :: (Serializable a, Serializable b)
      => Res
      -> ResGroup
      -> Spawn (Closure (CollectActor a b))
      -> DnaF (Shell (MR a) (Grp b))
    SpawnMappers
      :: (Serializable a, Serializable b)
      => Res
      -> ResGroup
      -> Spawn (Closure (Mapper a b))
      -> DnaF (Shell (Scatter a) (MR b))

    GADT_HADD(-- | Connect running actors)
    Connect
      :: (Serializable b, Typeable tag)
      => Shell a (tag b)
      -> Shell (tag b) c
      -> DnaF ()
    GADT_HADD(-- | Send parameter to the actor)
    SendParam
      :: Serializable a
      => a
      -> Shell (Val a) b
      -> DnaF ()

    GADT_HADD(-- | Delay actor returning single value)
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

-- | Spawn monad. It's used to carry all additional parameters for
--   process spawning
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
      -- ^ Crash with given probability. Not all actors will honor
      --   that request
    deriving (Show,Eq,Typeable,Generic)
instance Binary DebugFlag

runSpawn :: Spawn a -> (a,[SpawnFlag])
runSpawn (Spawn m) = runWriter m

useLocal :: Spawn ()
useLocal = Spawn $ tell [UseLocal]

failout :: Spawn ()
failout = Spawn $ tell [UseFailout]

timeout :: Double -> Spawn ()
timeout t = Spawn $ tell [UseTimeout t]

respawnOnFail :: Spawn ()
respawnOnFail = Spawn $ tell [UseRespawn]

debugFlags :: [DebugFlag] -> Spawn ()
debugFlags fs = Spawn $ tell [UseDebug fs]

newtype Promise a = Promise (ReceivePort a)

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


-- | Actors of this type are the counter-part to `CollectActor`: They
--   receive a single input message, but generate a set of output
--   messages. While 'CollectActor' corresponds to 'foldr', the
--   'Mapper' can be understood as the equivalent of 'unfoldr'.
data Mapper a b where
    Mapper :: (Serializable a, Serializable b, Serializable s)
           => (a -> IO s)
           -> (s -> IO (Maybe (s,b)))
           -> (Int -> b -> Int)
           -> Mapper a b
    deriving (Typeable)

-- | Just like a 'CollectActor', a 'Mapper' uses an internal state in
-- order to produce multiple values. However, the structure is
-- mirrored: Now the first parameter is a function that constructs the
-- internal state from the input value. Along the same lines, the
-- stepper function now iteratively produces an arbitrary number of
-- outputs, updating the state every time.  The third parameter
-- governs data distribution: Given a total number of possible
-- destinations and an output value, it specifies the index of the
-- destination to which it will be sent.
mapper :: (Serializable a, Serializable b, Serializable s)
       => (a -> IO s)              -- ^ initialization function
       -> (s -> IO (Maybe (s, b))) -- ^ chunking function
       -> (Int -> b -> Int)        -- ^ distribution function
       -> Mapper a b
mapper = Mapper



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

-- | Executes a kernel computation. This is the only way to escape the
-- 'DNA' monad in order to perform 'IO'. A bound thread will be
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

delay :: Serializable b => Location -> Shell a (Val b) -> DNA (Promise b)
delay loc sh = DNA $ singleton $ Delay loc sh

await :: Serializable a => Promise a -> DNA a
await = DNA . singleton . Await

delayGroup :: Serializable b => Shell a (Grp b) -> DNA (Group b)
delayGroup = DNA . singleton . DelayGroup

gatherM
    :: Serializable a
    => Group a
    -> (b -> a -> IO b)
    -> b
    -> DNA b
gatherM g f b = DNA $ singleton $ GatherM g f b

gather 
    :: Serializable a
    => Group a
    -> (b -> a -> b)
    -> b
    -> DNA b
gather g f = gatherM g (\b a -> return $ f b a)

sendParam :: Serializable a => a -> Shell (Val a) b -> DNA ()
sendParam a sh = DNA $ singleton $ SendParam a sh

connect :: (Serializable b, Typeable tag)
        => Shell a (tag b) -> Shell (tag b) c -> DNA ()
connect a b = DNA $ singleton $ Connect a b

-- | Broadcast same parameter to all actors in group
broadcast :: Shell (Scatter a) b -> Shell (Val a) b
broadcast (Shell a r s) = Shell a (RecvBroadcast r) s

availableNodes :: DNA Int
availableNodes = DNA $ singleton AvailNodes

-- | Evaluate actor without forking off enother thread
eval :: (Serializable a, Serializable b)
     => Actor a b
     -> a
     -> DNA b
eval (Actor act) = act

-- | Evaluate actor without forking off enother thread
evalClosure :: (Typeable a, Typeable b)
            => Closure (Actor a b)
            -> a
            -> DNA b
evalClosure clos a = DNA $ singleton $ EvalClosure a clos
    
startActor
    :: (Serializable a, Serializable b)
    => Res
    -> Spawn (Closure (Actor a b))
    -> DNA (Shell (Val a) (Val b))
startActor r a =
    DNA $ singleton $ SpawnActor r a


-- | Start single collector actor
startCollector :: (Serializable a, Serializable b)
               => Res
               -> Spawn (Closure (CollectActor a b))
               -> DNA (Shell (Grp a) (Val b))
startCollector res child =
    DNA $ singleton $ SpawnCollector res child

    -- (shellS,shellR) <- liftP newChan
    -- let clos = $(mkStaticClosure 'runCollectActor) `closureApply` child
    -- sendACP $ ReqSpawnShell clos shellS res
    -- msg <- unwrapMessage =<< liftP (receiveChan shellR)
    -- case msg of
    --   Nothing -> error "Bad shell message"
    --   Just  s -> return s


-- | Start group of processes
startGroup :: (Serializable a, Serializable b)
           => Res
           -> ResGroup   
           -> Spawn (Closure (Actor a b))
           -> DNA (Shell (Scatter a) (Grp b))
startGroup res resG child =
    DNA $ singleton $ SpawnGroup res resG child
    -- (shellS,shellR) <- liftP newChan
    -- let clos = $(mkStaticClosure 'runActor) `closureApply` child
    -- sendACP $ ReqSpawnGroup clos shellS res groupTy
    -- (gid,mbox) <- liftP (receiveChan shellR)
    -- msgs <- mapM unwrapMessage mbox
    -- case sequence msgs of
    --   Nothing -> error "Bad shell message"
    --   Just  s -> return $ assembleShellGroup gid s

-- | Start group of processes where we have more tasks then processes.
startGroupN
    :: (Serializable a, Serializable b)
    => Res         -- ^ Resources for actors
    -> ResGroup    -- ^
    -> Int
    -> Spawn (Closure (Actor a b))
    -> DNA (Shell (Val a) (Grp b))
startGroupN res resG nTasks child =
    DNA $ singleton $ SpawnGroupN res resG nTasks child

    -- (shellS,shellR) <- liftP newChan
    -- let clos = $(mkStaticClosure 'runActorManyRanks) `closureApply` child
    -- sendACP $ ReqSpawnGroupN clos shellS res nTasks groupTy
    -- (gid,mbox) <- liftP (receiveChan shellR)
    -- msgs <- mapM unwrapMessage mbox
    -- case sequence msgs of
    --   Nothing -> error "Bad shell message"
    --   Just  s -> return $ broadcast $ assembleShellGroup gid s

-- | Start group of collector processes
startCollectorGroup
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup   
    -> Spawn (Closure (CollectActor a b))
    -> DNA (Shell (Grp a) (Grp b))
startCollectorGroup res resG child =
    DNA $ singleton $ SpawnCollectorGroup res resG child

    -- (shellS,shellR) <- liftP newChan
    -- let clos = $(mkStaticClosure 'runCollectActor) `closureApply` child
    -- sendACP $ ReqSpawnGroup clos shellS res groupTy
    -- (gid,mbox) <- liftP (receiveChan shellR)
    -- msgs <- mapM unwrapMessage mbox
    -- case sequence msgs of
    --   Nothing -> error "Bad shell message"
    --   Just  s -> return $ assembleShellGroupCollect gid s

-- | Start group of collector processes
startCollectorGroupMR
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (CollectActor a b))
    -> DNA (Shell (MR a) (Grp b))
startCollectorGroupMR res resG child =
    DNA $ singleton $ SpawnCollectorGroupMR res resG child

    -- (shellS,shellR) <- liftP newChan
    -- let clos = $(mkStaticClosure 'runCollectActorMR) `closureApply` child
    -- sendACP $ ReqSpawnGroup clos shellS res groupTy
    -- (gid,mbox) <- liftP (receiveChan shellR)
    -- msgs <- mapM unwrapMessage mbox
    -- case sequence msgs of
    --   Nothing -> error "Bad shell message"
    --   Just  s -> return $ assembleShellGroupCollectMR gid s

-- | Start group of mapper processes
startMappers
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (Mapper a b))
    -> DNA (Shell (Scatter a) (MR b))
startMappers res resG child =
    DNA $ singleton $ SpawnMappers res resG child
    -- (shellS,shellR) <- liftP newChan
    -- let clos = $(mkStaticClosure 'runMapperActor) `closureApply` child
    -- sendACP $ ReqSpawnGroup clos shellS res groupTy
    -- (gid,mbox) <- liftP (receiveChan shellR)
    -- msgs <- mapM unwrapMessage mbox
    -- case sequence msgs of
    --   Nothing -> error "Bad shell message"
    --   Just  s -> return $ assembleShellMapper gid s
