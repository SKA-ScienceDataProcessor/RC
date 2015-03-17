{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
-- | Data types for interpretation of DNA DSL using cloud haskell
module DNA.Interpreter.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Operational
import Control.Concurrent.Async
import Control.Concurrent.STM (STM)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Data.Monoid
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
import           Data.Set   (Set)
import GHC.Generics  (Generic)

import DNA.Types
import DNA.Lens
import DNA.DSL


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Type synonym for monad which is used for interpretation of
type DnaMonad = StateT StateDNA Process

-- | Monad for event handlers. It adds explicit error handling
type Controller = ExceptT String DnaMonad

fatal :: MonadError String m => String -> m a
fatal = throwError

-- | Run controller monad. On failure will terminate process forefully
runController :: Controller a -> DnaMonad a
runController m = do
    r <- runExceptT m
    case r of
      Left  e -> error e -- FIXME: better error handling (legging, etc)
      Right a -> return a

-- | Run DNA monad
runDnaMonad
    :: Rank
    -> GroupSize
    -> Closure DnaInterpreter
    -> [NodeId]
    -> DnaMonad a
    -> Process a
runDnaMonad (Rank rnk) (GroupSize grp) interp nodes =
    flip evalStateT s0
  where
    s0 = StateDNA
           { _stCounter   = 0
           , _stRank      = rnk
           , _stGroupSize = grp
           , _stInterpreter = interp
           , _stNodePool      = Set.fromList nodes
           , _stUsedResources = Map.empty
           , _stChildren      = Map.empty
           , _stGroups        = Map.empty
           }

-- Generate unique ID
uniqID :: Monad m => StateT StateDNA m Int
uniqID = do
    i <- use stCounter
    stCounter .= (i + 1)
    return i


-- | Evaluator for DNA monad. We have to pass closure with function
--   for interpreting DNA monad manually since we have to break
--   dependency loop. We also want to wrap function into
newtype DnaInterpreter = DnaInterpreter { dnaInterpreter :: forall a. DNA a -> DnaMonad a }
                       deriving Typeable


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Parameter send to actor on startup
data ActorParam = ActorParam
    { actorParent      :: ProcessId
      -- ^ Parent of an actor
    , actorInterpreter :: Closure DnaInterpreter
      -- ^ Interpreter for DNA DSL
    , actorRank        :: Rank
      -- ^ Rank of an actor
    , actorGroupSize   :: GroupSize
      -- ^ Size of group of actors
    , actorNodes       :: [NodeId]
    }
    deriving (Typeable,Generic)
instance Binary ActorParam


-- | State of interpreter
data StateDNA = StateDNA
    { _stCounter   :: !Int
      -- Counter for generation of unique IDs
    , _stRank        :: !Int
    , _stGroupSize   :: !Int
    , _stInterpreter :: !(Closure DnaInterpreter)
      
    , _stNodePool      :: !(Set NodeId)
      -- ^ Unused nodes which could be reused
    , _stUsedResources :: !(Map ProcessId VirtualCAD)
      -- ^ Resources used by some process

      -- ^ All available nodes and their status
    , _stChildren :: !(Map ProcessId (Either ProcState GroupID))
      -- ^ State of monitored processes
    , _stGroups   :: !(Map GroupID GroupState)
      -- ^ State of groups of processes
    }

-- | State of child process.
data ProcState
    = ShellProc (SendPort Message)
      -- | We started process but didn't receive status update from it
    | Unconnected
      -- | Process is running but we don't know its sink yet
    | Connected [ProcessId]
      -- | Process is running and we know its sink[s]
    | Failed
      -- | Process failed
    deriving (Show)


-- | State of group of processes
data GroupState
    =  GrUnconnected GroupType (Int,Int)
      -- Group which is not connected yet
      --  + Type of group
      --  + (N running processes, N completed[hack])
    | GrConnected GroupType (Int,Int) [SendPort Int] [ProcessId]
      -- Connected group
      --  + Type of group
      --  + (N running, N completed)
      --  + Destinations
    | GrFailed
      -- Group which crashed
    deriving (Show)



----------------------------------------------------------------
-- Lens and combinators
----------------------------------------------------------------


stCounter :: Lens' StateDNA Int
stCounter = lens _stCounter (\a x -> x { _stCounter = a})

stRank :: Lens' StateDNA Int
stRank = lens _stRank (\a x -> x { _stRank = a})

stGroupSize :: Lens' StateDNA Int
stGroupSize = lens _stGroupSize (\a x -> x { _stCounter = a})

stInterpreter :: Lens' StateDNA (Closure DnaInterpreter)
stInterpreter = lens _stInterpreter (\a x -> x { _stInterpreter = a})

stChildren :: Lens' StateDNA (Map ProcessId (Either ProcState GroupID))
stChildren = lens _stChildren (\a x -> x { _stChildren = a})

stGroups :: Lens' StateDNA (Map GroupID GroupState)
stGroups = lens _stGroups (\a x -> x { _stGroups = a})

stNodePool :: Lens' StateDNA (Set NodeId)
stNodePool = lens _stNodePool (\a x -> x { _stNodePool = a})

stUsedResources :: Lens' StateDNA (Map ProcessId VirtualCAD)
stUsedResources = lens _stUsedResources (\a x -> x { _stUsedResources = a})

-- Process event where we dispatch on PID of process
handlePidEvent
    :: ProcessId
    -> (Controller ())
    -- What to do when PID not found
    -> (ProcState  -> Controller (Maybe ProcState))
    -- What to do with single process
    -> (GroupState -> GroupID -> Controller (Maybe GroupState))
    -- What to do with group of processes
    -> Controller ()
handlePidEvent pid none onProc onGrp = do
    r <- use $ stChildren . at pid
    case r of
      Nothing          -> none
      Just (Left  p  ) -> do mp' <- onProc p
                             case mp' of
                               Nothing -> dropPID pid
                               Just p' -> stChildren . at pid .= Just (Left p')
      Just (Right gid) -> do Just g <- use $ stGroups . at gid
                             mg' <- onGrp g gid
                             case mg' of
                               Nothing -> dropGroup gid
                               Just g' -> stGroups . at gid .= Just g'

-- Remove PID from process registry
dropPID :: ProcessId -> Controller ()
dropPID pid = do
    stChildren . at pid .= Nothing
    mr <- use $ stUsedResources . at pid
    case mr of
      Nothing -> return ()
      Just (VirtualCAD Local  _ ns) -> stNodePool %= (Set.fromList ns <>)
      Just (VirtualCAD Remote n ns) -> stNodePool %= (\xs -> Set.singleton n <> Set.fromList ns <> xs)

-- Remove GID from process registry
dropGroup :: GroupID -> Controller ()
dropGroup gid = do
    stGroups      . at gid .= Nothing
    -- stPooledProcs . at gid .= Nothing
    -- stCountRank   . at gid .= Nothing
    children <- use stChildren
    let pids = [p | (p,Right gid') <- Map.toList children
                  , gid' == gid
                  ]
    forM_ pids dropPID

getGroupPids :: GroupID -> Controller [ProcessId]
getGroupPids gid = do
    ch <- use stChildren
    return [ pid | (pid, Right g) <- Map.toList ch
                 , g == gid
                 ]

terminateGroup :: GroupID -> Controller ()
terminateGroup gid = do
    pids <- getGroupPids gid
    liftP $ forM_ pids $ \p -> send p Terminate
