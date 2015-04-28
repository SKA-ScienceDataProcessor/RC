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
    -- , startGroupN
    , startCollectorGroup
    -- , startCollectorGroupMR
    -- , startMappers
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

-- | GADT which describe operations supported by DNA DSL
data DnaF a where
    -- | Execute foreign kernel
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

    -- | Evaluate actor's closure
    EvalClosure
      :: (Typeable a, Typeable b)
      => a
      -> Closure (Actor a b)
      -> DnaF b
    -- | Spawn single process
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
    -- SpawnCollectorGroupMR
    --   :: (Serializable a, Serializable b)
    --   => Res
    --   -> ResGroup
    --   -> Spawn (Closure (CollectActor a b))
    --   -> DnaF (Shell (MR a) (Grp b))
    -- SpawnMappers
    --   :: (Serializable a, Serializable b)
    --   => Res
    --   -> ResGroup
    --   -> Spawn (Closure (Mapper a b))
    --   -> DnaF (Shell (Scatter a) (MR b))

    -- | Connect running actors
    Connect
      :: (Serializable b, Typeable tag)
      => Shell a (tag b)
      -> Shell (tag b) c
      -> DnaF ()
    -- | Send parameter to the actor
    SendParam
      :: Serializable a
      => a
      -> Shell (Val a) b
      -> DnaF ()
    -- | Send parameter to the actor
    Broadcast
      :: Serializable a
      => a
      -> Shell (Scatter a) b
      -> DnaF ()

    -- | Delay actor returning single value
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

-- | Actor which receive messages of type @a@ and produce result of
--   type @b@. It's phantom-typed and could only be constructed by
--   'actor' which ensures that types are indeed correct.
data Actor a b where
    Actor :: (Serializable a, Serializable b) => (a -> DNA b) -> Actor a b
    deriving (Typeable)

-- | Smart constructor for actors. Here we receive parameters and
--   output channel for an actor
actor :: (Serializable a, Serializable b)
      => (a -> DNA b)
      -> Actor a b
actor = Actor

-- | Actor which collects multiple inputs from other actors
data CollectActor a b where
    CollectActor :: (Serializable a, Serializable b)
                 => (s -> a -> IO s)
                 -> IO s
                 -> (s -> IO b)
                 -> CollectActor a b
    deriving (Typeable)

-- | Smart constructor for collector actors.
collectActor
    :: (Serializable a, Serializable b, Serializable s)
    => (s -> a -> IO s)
    -> IO s
    -> (s -> IO b)
    -> CollectActor a b
collectActor = CollectActor


-- | Mapper actor. Essentially unfoldr
data Mapper a b where
    Mapper :: (Serializable a, Serializable b, Serializable s)
           => (a -> IO s)
           -> (s -> IO (Maybe (s,b)))
           -> (Int -> b -> Int)
           -> Mapper a b
    deriving (Typeable)

mapper :: (Serializable a, Serializable b, Serializable s)
       => (a -> IO s)
       -> (s -> IO (Maybe (s, b)))
       -> (Int -> b -> Int)
       -> Mapper a b
mapper = Mapper



----------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------

rank :: DNA Int
rank = DNA $ singleton DnaRank

groupSize :: DNA Int
groupSize = DNA $ singleton DnaGroupSize

kernel :: String -> [ProfileHint] -> Kern a -> DNA a
kernel msg hints = DNA . singleton . Kernel msg BoundKernel hints

unboundKernel :: String -> [ProfileHint] -> Kern a -> DNA a
unboundKernel msg hints = DNA . singleton . Kernel msg DefaultKernel hints

logMessage :: String -> DNA ()
logMessage = DNA . singleton . LogMessage

duration :: String -> DNA a -> DNA a
duration msg = DNA . singleton . Duration msg

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

broadcast :: Serializable a => a -> Shell (Scatter a) b -> DNA ()
broadcast a sh = DNA $ singleton $ Broadcast a sh

connect :: (Serializable b, Typeable tag)
        => Shell a (tag b) -> Shell (tag b) c -> DNA ()
connect a b = DNA $ singleton $ Connect a b

-- -- | Broadcast same parameter to all actors in group
-- broadcast :: Shell (Scatter a) b -> Shell (Val a) b
-- broadcast = undefined -- (Shell a) = (Shell __ __ _ _ _ _ _  r s) = Shell a (RecvBroadcast r) s


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

-- | Start group of processes
startGroup :: (Serializable a, Serializable b)
           => Res
           -> ResGroup   
           -> Spawn (Closure (Actor a b))
           -> DNA (Shell (Scatter a) (Grp b))
startGroup res resG child =
    DNA $ singleton $ SpawnGroup res resG child

-- -- | Start group of processes where we have more tasks then processes.
-- startGroupN
--     :: (Serializable a, Serializable b)
--     => Res         -- ^ Resources for actors
--     -> ResGroup    -- ^
--     -> Int
--     -> Spawn (Closure (Actor a b))
--     -> DNA (Shell (Val a) (Grp b))
-- startGroupN res resG nTasks child =
--     DNA $ singleton $ SpawnGroupN res resG nTasks child

-- | Start group of collector processes
startCollectorGroup
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup   
    -> Spawn (Closure (CollectActor a b))
    -> DNA (Shell (Grp a) (Grp b))
startCollectorGroup res resG child =
    DNA $ singleton $ SpawnCollectorGroup res resG child

{-
-- | Start group of collector processes
startCollectorGroupMR
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (CollectActor a b))
    -> DNA (Shell (MR a) (Grp b))
startCollectorGroupMR res resG child =
    DNA $ singleton $ SpawnCollectorGroupMR res resG child
-}

{-
-- | Start group of mapper processes
startMappers
    :: (Serializable a, Serializable b)
    => Res
    -> ResGroup
    -> Spawn (Closure (Mapper a b))
    -> DNA (Shell (Scatter a) (MR b))
startMappers res resG child =
    DNA $ singleton $ SpawnMappers res resG child
-}
