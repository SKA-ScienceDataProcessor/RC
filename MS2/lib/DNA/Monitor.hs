{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Goal of monitor process is to watch over child processes and track
-- nodes allocated to child processes. Every actor have corresponding
-- monitor process. We need separate monitor process because actor
-- could do lengthy computation and thus unable to respond to request
-- about stat of processes in timely manner. Use of one monitor per
-- actor also make many problems of cleanup much easier to solve.
module DNA.Monitor (
      -- * Monitor API
      Monitor
    , runMonitor
      -- * Scheduling
    , ReqNode(..)
    , ReqGroup(..)
    , ActorType(..)
    , NodePool(..)
    , Nodes(..)
    , askSingleNode
    , askNodePool
    , askNodeGroup
      -- * Register process for monitoring
    , registerWorker
    , registerGroup
    , registerFailout
      -- * Ask for process state
    , waitForProcess
    , waitForGroup
    ) where


import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Distributed.Process

import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import Data.Functor.Identity
import qualified Data.Set        as Set
import           Data.Set          (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map)
import qualified Data.Foldable   as T
import GHC.Generics  (Generic)

import DNA.Types


----------------------------------------------------------------
-- Monitor API
----------------------------------------------------------------

-- | Handle for monitor of processes
newtype Monitor = Monitor ProcessId
                deriving (Show,Eq,Typeable,Generic,Binary)

-- | Request for single node for child process.
data ReqNode = ReqNode ActorType NodePool
             deriving (Show,Eq,Typeable,Generic)

-- | Request nodes for group of processes.
data ReqGroup = ReqGroup
              deriving (Show,Eq,Typeable,Generic)

-- | Whether actor is computationally expensive of cheap
data ActorType
    = Expensive  -- ^ Actor uses node exclusively.
    | Cheap      -- ^ Actor is cheap. E.g. most of the time waits for
                 --   messages and does very small amount of
                 --   computations.
    deriving (Show,Eq,Typeable,Generic)

-- | How much of node pool should we transfer to our child process.
data NodePool
    = AllNodePool -- ^ Give whole pool to child process
    | NoNodes     -- ^ Don't give any nodes to child
    deriving (Show,Eq,Typeable,Generic)

-- | List of nodes which could be used by an actor
data Nodes = Nodes
    { mainNode       :: NodeId     -- ^ Node on which process is executed
    , ownedNodes     :: [NodeId]   -- ^ Nodes owned by actor
    }
    deriving (Show,Eq,Typeable,Generic)

instance Binary ReqNode
instance Binary ReqGroup
instance Binary NodePool
instance Binary ActorType
instance Binary Nodes

-- | Ask monitor for nodes for single actor. It returns schedule for
--   the node and
askSingleNode :: Monitor -> ReqNode -> Process (ActorID,Nodes)
askSingleNode (Monitor mon) req = do
    me <- getSelfPid
    send mon (me,req)
    mr <- expect
    case mr of
      Nothing -> error "Cannot find node"
      Just  r -> return r

-- | Ask monitor for nodes for group of actors.
--
--  FIXME: we simply give all free nodes and don't assign any owned
--         nodes
askNodeGroup :: Monitor -> ReqGroup -> Process (GroupID,[Nodes])
askNodeGroup (Monitor mon) req = do
    me <- getSelfPid
    send mon (me,req)
    mr <- expect
    case mr of
      Nothing -> error "Cannot schedule nodes"
      Just r  -> return r

-- | Ask for pool of nodes for locally spawned actor
askNodePool :: Monitor -> NodePool -> Process (ActorID,[NodeId])
askNodePool (Monitor mon) pool = do
    me <- getSelfPid
    send mon (me,pool)
    mr <- expect
    case mr of
      Nothing -> error "Cannot schedule nodes"
      Just r  -> return r


-- | Register single worker process.
registerWorker :: Monitor -> ActorID -> ProcessId -> Process ()
registerWorker (Monitor mon) aid pid = do
    send mon (RegisterPID aid pid)

-- | Register process in group of processes.
registerGroup :: Monitor -> GroupID -> [ProcessId] -> Process ()
registerGroup (Monitor mon) gid pids = do
    send mon (RegisterGroup gid pids)

-- | Register process in group of failout processes
registerFailout :: Monitor -> GroupID -> [ProcessId] -> Process ()
registerFailout (Monitor mon) gid pids  = do
    send mon (RegisterFailout gid pids)

-- | Ask about process termination.
waitForProcess :: Monitor -> ProcessId -> Process (ReceivePort ())
waitForProcess (Monitor mon) pid = do
    (chSend,chRecv) <- newChan
    send mon (AskProcess pid chSend)
    return chRecv

-- | Ask about process termination.
waitForGroup :: Monitor -> GroupID -> Process (ReceivePort (Maybe Int))
waitForGroup (Monitor mon) gid = do
    (chSend,chRecv) <- newChan
    send mon (AskGroup gid chSend)
    return chRecv



----------------------------------------------------------------
-- Internal message types
----------------------------------------------------------------

-- Register PID of single process
data RegisterPID = RegisterPID ActorID ProcessId
                deriving (Show,Eq,Typeable,Generic)

-- Register group of processes
data RegisterGroup = RegisterGroup GroupID [ProcessId]
                deriving (Show,Eq,Typeable,Generic)

-- Register group of processes which use failout fault tolerance
-- strategy
data RegisterFailout = RegisterFailout GroupID [ProcessId]
                deriving (Show,Eq,Typeable,Generic)

-- Ask about process status. It contain PID of process we're interesed
-- in and port to send failure bit
data AskProcess = AskProcess ProcessId (SendPort ())
                deriving (Show,Eq,Typeable,Generic)

-- Ask about status of group. It contain Group ID and post to send
-- back number of failed processes.
data AskGroup = AskGroup GroupID (SendPort (Maybe Int))
                deriving (Show,Eq,Typeable,Generic)

instance Binary AskProcess
instance Binary AskGroup
instance Binary RegisterPID
instance Binary RegisterGroup
instance Binary RegisterFailout



----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

-- | Start execution of monitor process
runMonitor :: [NodeId] -> Process Monitor
runMonitor nids = do
    forM_ nids monitorNode
    pid <- spawnLocal $ iterateM step S { _stCounter       = 0
                                        , _stWorkers       = Map.empty
                                        , _stGroups        = Map.empty
                                        , _stFreeNodes     = Set.fromList nids
                                        , _stBusyNodes     = Map.empty
                                        , _stPendingNodes  = Map.empty
                                        , _stPendingGroups = Map.empty
                                        }
    return $ Monitor pid

-- Iterate monadict action indefinitelys
iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = loop a
   where
     loop = f >=> loop


-- Single step of monitor
step :: S -> Process S
step st = receiveWait
    [ -- Requests for nodes
      msg handleReqProcess
    , msg handleReqGroup
    , msg handleReqPool
      -- Monitoring notifications
    , msg $ \(ProcessMonitorNotification _ pid reason) -> do
        case reason of
          DiedUnknownId -> error "FIXME: not clear how to handle such case"
          DiedNormal    -> handleNormalTermination pid
          _             -> handleProcessCrash      pid
    , msg $ \(NodeMonitorNotification _ nid _) ->
              handleNodeCrash nid
      -- Register processes
    , msg handleRegisterProcess
    , msg handleRegisterGroup
    , msg handleRegisterFailout
      -- Ask for process state
    , msg handleAskProcess
    , msg handleAskGroup
    ]
  where
     msg handler = match $ \a -> execStateT (handler a) st

-- Request nodes for single process
handleReqProcess :: (ProcessId,ReqNode) -> StateT S Process ()
handleReqProcess (pid,ReqNode actorType pool) = do
    aid  <- ActorID <$> uniqID
    free <- use stFreeNodes
    msg  <- case (actorType,pool) of
      -- Expensive process
      (Expensive,NoNodes    ) -> runMaybeT $ do
          (n,ns) <- MaybeT $ return $ Set.maxView free
          stFreeNodes .= ns
          return $ Nodes n []
      (Expensive,AllNodePool) -> runMaybeT $ do
          (n,ns) <- MaybeT $ return $ Set.maxView free
          stFreeNodes .= Set.empty
          return $ Nodes n (T.toList ns)
      -- Cheap process
      --
      -- FIXME: we could use self process if none available
      (Cheap,    NoNodes    ) -> case Set.maxView free of
          Nothing     -> return Nothing
          Just (n,ns) -> do stFreeNodes .= ns
                            return $ Just $ Nodes n []
      (Cheap,    AllNodePool) -> case Set.maxView free of
          Nothing     -> return Nothing
          Just (n,ns) -> do stFreeNodes .= Set.empty
                            return $ Just $ Nodes n (T.toList ns)
    lift $ send pid $ (,) aid <$> msg

handleReqPool :: (ProcessId,NodePool) -> StateT S Process ()
handleReqPool (pid,pool) = do
    aid  <- ActorID <$> uniqID
    free <- use stFreeNodes
    case pool of
      NoNodes     -> lift $ send pid (aid, [] :: [NodeId])
      AllNodePool -> do lift $ send pid (aid, T.toList free)
                        stFreeNodes .= Set.empty

-- Request nodes for group of processes
handleReqGroup :: (ProcessId,ReqGroup) -> StateT S Process ()
handleReqGroup (pid,ReqGroup) = do
    gid  <- GroupID <$> uniqID
    free <- use stFreeNodes
    msg  <- case () of
        _| Set.null free -> return Nothing
         | otherwise     -> do stFreeNodes .= Set.empty
                               return $ Just (gid,[Nodes n []
                                                  | n <- T.toList free ])
    lift $ send pid msg

-- Handle normal termination of process
handleNormalTermination :: ProcessId -> StateT S Process ()
handleNormalTermination pid = do
    r <- use $ stWorkers . at pid
    case r of
      Nothing -> return ()
      Just (Left _)    -> stWorkers . at pid .= Nothing
      Just (Right gid) -> do
          Just g <- use $ stGroups . at gid
          stWorkers . at pid .= Nothing
          case g of
            Group ps mch -> case Set.delete pid ps of
              ps' | Set.null ps' -> stGroups . at gid .= Nothing
                  | otherwise    -> stGroups . at gid .= Just (Group ps' mch)
            Failout ps mch n -> case Set.delete pid ps of
              ps' | Set.null ps' && n == 0 -> stGroups . at gid .= Nothing
                  | Set.null ps' -> case mch of
                      Just ch -> do lift $ sendChan ch (Just n)
                                    stGroups . at gid .= Nothing
                      Nothing -> stGroups . at gid .= Just (CompletedGroup (Just n))
                  | otherwise -> stGroups . at gid .= Just (Failout ps' mch n)
            CompletedGroup _ -> return ()

-- Handle abnormal termination of a process
handleProcessCrash :: ProcessId -> StateT S Process ()
handleProcessCrash pid = do
    r <- use $ stWorkers . at pid
    case r of
      Nothing -> return ()
      Just (Left (Awaited ch)) -> do lift $ sendChan ch ()
                                     stWorkers %= Map.delete pid
      Just (Left _) -> stWorkers . at pid .= Just (Left Failed)
      Just (Right gid) -> do
          Just g <- use $ stGroups . at gid
          case g of
            Group ps mch -> do
                stWorkers %= (\m -> T.foldr Map.delete m ps)
                case mch of
                  Just ch -> do lift $ sendChan ch Nothing
                                stGroups . at gid .= Nothing
                  Nothing -> stGroups . at gid .= Just (CompletedGroup Nothing)
            Failout ps mch n -> case Set.delete pid ps of
                ps' | Set.null ps' -> case mch of
                        Just ch -> do lift $ sendChan ch (Just (n+1))
                                      stGroups . at gid .= Nothing
                        Nothing -> stGroups . at gid .= Just (CompletedGroup (Just (n+1)))
                    | otherwise -> stGroups . at gid .= Just (Failout ps' mch (n+1))
            CompletedGroup _ -> return ()

-- Remove all mentions of nodes
handleNodeCrash :: NodeId -> StateT S Process ()
handleNodeCrash nid = do
    stBusyNodes     %= fmap (Set.delete nid)
    stPendingNodes  %= fmap (Set.delete nid)
    stPendingGroups %= (fmap . fmap) (Set.delete nid)

-- Register single process
handleRegisterProcess :: RegisterPID -> StateT S Process ()
handleRegisterProcess (RegisterPID aid pid) = do
    -- Set up PID for monitoring
    Just nids <- use $ stPendingNodes . at aid
    stPendingNodes . at aid .= Nothing
    stBusyNodes    . at pid .= Just nids
    _ <- lift $ monitor pid
    lift $ send pid Completed


-- Register group of processes
handleRegisterGroup :: RegisterGroup -> StateT S Process ()
handleRegisterGroup (RegisterGroup gid pids) = do
    -- Set up PIDs for monitoring
    stGroups . at gid .= Just (Group (Set.fromList pids) Nothing)
    forM_ pids $ \p -> do
        stWorkers . at p .= Just (Right gid)
        _ <- lift $ monitor p
        lift $ send p Completed
    -- Put nodes into list of running processes
    Just nidSets <- use $ stPendingGroups . at gid
    stPendingGroups . at gid .= Nothing
    forM_ (zipSame pids nidSets) $ \(p,ns) ->
        stBusyNodes . at p .= Just ns


-- Register failout group of processes
handleRegisterFailout :: RegisterFailout -> StateT S Process ()
handleRegisterFailout (RegisterFailout gid pids) = do
    -- Set up PIDs for monitoring
    stGroups . at gid .= Just (Failout (Set.fromList pids) Nothing 0)
    forM_ pids $ \p -> do
        stWorkers . at p .= Just (Right gid)
        _ <- lift $ monitor p
        lift $ send p Completed
    -- Put nodes into list of running processes
    Just nidSets <- use $ stPendingGroups . at gid
    stPendingGroups . at gid .= Nothing
    forM_ (zipSame pids nidSets) $ \(p,ns) ->
        stBusyNodes . at p .= Just ns


-- Ask about status of single actor
handleAskProcess :: AskProcess -> StateT S Process ()
handleAskProcess (AskProcess pid ch) = do
    r <- use $ stWorkers . at pid
    case r of
      Nothing -> return ()
      -- Could happen during double await!
      Just (Left Awaited{}) -> error "SHOULD NOT HAPPEN"
      Just (Left Failed)    -> do lift $ sendChan ch ()
                                  stWorkers . at pid .= Nothing
      Just (Left Running)   -> stWorkers . at pid .= Just (Left (Awaited ch))
      Just (Right _)        -> error "SHOULD NOT HAPPEN"

-- Ask about status of group of actors
handleAskGroup :: AskGroup -> StateT S Process ()
handleAskGroup (AskGroup gid ch) = do
    r <- use $ stGroups . at gid
    case r of
      Nothing -> return ()
      Just (CompletedGroup x) -> do lift $ sendChan ch x
                                    stGroups . at gid .= Nothing
      Just (Group   pids _  ) -> stGroups . at gid .= Just (Group pids (Just ch))
      Just (Failout pids _ n) -> stGroups . at gid .= Just (Failout pids (Just ch) n)

-- Generate unique ID
uniqID :: Monad m => StateT S m Int
uniqID = do
    i <- use stCounter
    stCounter .= (i + 1)
    return i

-- Zip two lists of equal length
zipSame :: [a] -> [b] -> [(a,b)]
zipSame []     []     = []
zipSame (a:as) (b:bs) = (a,b) : zipSame as bs
zipSame _ _ = error "Lists have unequal length"


----------------------------------------------------------------
-- Monitoring of processes
----------------------------------------------------------------

-- State of monitor process. We wrap everything into single process.
data S = S
    { _stCounter :: !Int
      -- Counter for generating unique IDs
    , _stWorkers :: !(Map ProcessId (Either ProcState GroupID))
      -- State of monitored processes
    , _stGroups  :: !(Map GroupID GroupState)
      -- State of groups being monitored
    , _stFreeNodes :: !(Set NodeId)
      -- Nodes which are not used
    , _stBusyNodes :: !(Map ProcessId (Set NodeId))
      -- Nodes which are in use by other processes
    , _stPendingNodes :: !(Map ActorID (Set NodeId))
      -- Nodes which are sent to the main process but we didn't
      -- receive confirmation yet.
    , _stPendingGroups :: !(Map GroupID [Set NodeId])
      -- Node group which is sent to main process but we didn't
      -- receive confirmation yet.
    }

-- State process group
data GroupState
    = Group   (Set ProcessId) (Maybe (SendPort (Maybe Int)))
      -- Group of normal processes.
    | Failout (Set ProcessId) (Maybe (SendPort (Maybe Int))) Int
      -- Group of failout processes. It keep number of failed processes
    | CompletedGroup (Maybe Int)
      -- Group completed execution and there was failures

-- State of monitored process
data ProcState
    = Running
      -- Process is still running and no one asked for its state
    | Awaited (SendPort ())
      -- Someone is asked for process state
    | Failed
      -- Process crashed before someone asked about it


stCounter :: Lens' S Int
stCounter = lens _stCounter (\a x -> x { _stCounter = a})

stWorkers :: Lens' S (Map ProcessId (Either ProcState GroupID))
stWorkers = lens _stWorkers (\a x -> x { _stWorkers = a})

stGroups :: Lens' S (Map GroupID GroupState)
stGroups = lens _stGroups (\a x -> x { _stGroups = a})

stFreeNodes :: Lens' S (Set NodeId)
stFreeNodes = lens _stFreeNodes (\a x -> x { _stFreeNodes = a})

stBusyNodes :: Lens' S (Map ProcessId (Set NodeId))
stBusyNodes = lens _stBusyNodes (\a x -> x { _stBusyNodes = a})

stPendingNodes :: Lens' S (Map ActorID (Set NodeId))
stPendingNodes = lens _stPendingNodes (\a x -> x { _stPendingNodes = a})

stPendingGroups :: Lens' S (Map GroupID [Set NodeId])
stPendingGroups = lens _stPendingGroups (\a x -> x { _stPendingGroups = a})



----------------------------------------------------------------
-- Lens
--
-- Lens are immensely useful for working with state but I don't want
-- to pull in full Kmettoverse for small set of combinators.
--
-- Here we redefine all necessary combinators. Full compatibility with
-- lens is maintained.

type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

lens :: (s -> a) -> (a -> s -> s) -> Lens' s a
lens getf putf = \f s -> flip putf s <$> f (getf s)

-- Get value from object
(^.) :: s -> Lens' s a -> a
s ^. l = getConst $ l Const s

-- Put value into object
set :: Lens' s a -> a -> s -> s
set l a s = runIdentity $ l (\_ -> Identity a) s

over :: Lens' s a -> (a -> a) -> s -> s
over l f s = runIdentity $ l (Identity . f) s

(.=) :: MonadState s m => Lens' s a -> a -> m ()
l .= b = modify' $ set l b

(%=) :: MonadState s m => Lens' s a -> (a -> a) -> m ()
l %= b = modify' $ over l b

infix 4 .=, %=

use :: MonadState s m => Lens' s a -> m a
use l = do
    s <- get
    return $ s ^. l


at :: (Ord k) => k -> Lens' (Map k v) (Maybe v)
at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (Map.delete k m)) mv
    Just v' -> Map.insert k v' m
    where mv = Map.lookup k m
{-# INLINE at #-}


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
