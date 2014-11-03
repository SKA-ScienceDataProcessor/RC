{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Process which monitor execution of all other processes. We use
-- dedicated process for monitoring. One natural alternative is let
-- parents to monitor their children. It however won't work for
-- following reasons:
--
--  * Process which receive result of computation should get either
--    result or notification that process failed. Parent could be busy
--    doing computations and won't be able to answer in timely manner.
--
--  * Worker process could in principle outlive its parent so parent
--    could not reasonable monitor it.
--
-- FIXME: data race during process registration. Process could
--        complete execution either normally or abnormally before we
--        register it succesfully. Reasonable way to fix this is to
--        send message to a process and require it to receive it. This
--        way if we get notification that tell that process is unknown
--        we know that it crashed.
--
-- FIXME: Main scheduler should live here as well because we need to
--        return nodes to parent even if process died (and node is
--        still alive). Parent doesn't monitor child so it doesn't
--        know their status.
module DNA.Monitor (
      -- * Monitor API
      Monitor
    , runMonitor
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
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Distributed.Static (closureApply)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe
import Control.Distributed.Process.Platform.ManagedProcess
import Control.Distributed.Process.Serializable (Serializable)

import Data.Binary   (Binary)
import Data.Int
import Data.Typeable (Typeable)
import Data.Monoid   (Monoid(..))
import qualified Data.Set        as Set
import           Data.Set          (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map,(!))
import qualified Data.Foldable   as T
import GHC.Generics  (Generic)

import DNA.Logging
import DNA.Types


----------------------------------------------------------------
-- External API
--
-- When process awaits result from process/group of processes (worker)
-- it asks monitor to notify it about failures of worker. If
-- process terminated normally monitor will not respond and process
-- will obtain result normally. If worker died to any reason monitor will
-- notify process about it.
----------------------------------------------------------------

-- | Handle for monitor of processes
newtype Monitor = Monitor ProcessId
                deriving (Show,Eq,Typeable,Generic,Binary)

-- Register PID of single process
newtype RegisterPID = RegisterPID ProcessId
                deriving (Show,Eq,Typeable,Generic,Binary)

-- Register group of processes
data RegisterGroup = RegisterGroup ProcessId [ProcessId]
                deriving (Show,Eq,Typeable,Generic)

-- Register group of processes which use failout fault tolerance
-- strategy
data RegisterFailout = RegisterFailout ProcessId [ProcessId]
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
instance Binary RegisterGroup
instance Binary RegisterFailout



-- | Register single worker process.
registerWorker :: Monitor -> ProcessId -> Process ()
registerWorker (Monitor mon) pid = do
    send mon (RegisterPID pid)

-- | Register process in group of processes.
registerGroup :: Monitor -> [ProcessId] -> Process GroupID
registerGroup (Monitor mon) pids = do
    me <- getSelfPid
    send mon (RegisterGroup me pids)
    expect


-- | Register process in group of failout processes
registerFailout :: Monitor -> [ProcessId] -> Process GroupID
registerFailout (Monitor mon) pids  = do
    me <- getSelfPid
    send mon (RegisterFailout me pids)
    expect

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
-- Implementation
----------------------------------------------------------------

-- | Start execution of monitor process
runMonitor :: Process Monitor
runMonitor = do
    pid <- spawnLocal $ iterateM step (0, MonitorSt Map.empty Map.empty)
    return $ Monitor pid

-- Internal state of process monitor
data MonitorSt = MonitorSt
  { monWorkers :: !(Map ProcessId (Either ProcState GroupID))
    -- Single process
  , monGroups  :: !(Map GroupID GroupState)
    -- State of process groups
  }

-- State process group
data GroupState
    = Group   (Set ProcessId) (Maybe (SendPort (Maybe Int)))
      -- Group of normal processes.
    | Failout (Set ProcessId) (Maybe (SendPort (Maybe Int))) Int
      -- Group of failout processes. It keep number of failed processes
    | FailedGroup
      -- Execution of process group failed but no process asked about it

-- State of monitored process
data ProcState
    = Running
      -- Process is still running and no one asked for its state
    | Awaited (SendPort ())
      -- Someone is asked for process state
    | Failed
      -- Process crashed before someone asked about it


-- Single step of monitor
step :: (Int,MonitorSt) -> Process (Int,MonitorSt)
step (i,st) = receiveWait
  [ match $ \(ProcessMonitorNotification _ pid reason) -> case reason of
                DiedUnknownId -> error "FIXME: not clear how to handle such case"
                DiedNormal    -> (i,) <$> handleNormalTermination st pid
                _             -> (i,) <$> handleCrash             st pid
  , match $ \x -> (i,) <$> handleAskProcess      st x
  , match $ \x -> (i,) <$> handleAskGroup        st x
  , match $ \x -> (i,) <$> handleRegisterProc    st x
  , match $ handleRegisterGroup   (i,st)
  , match $ handleRegisterFailout (i,st)
  ]

-- Handle normal termination of the process
handleNormalTermination :: MonitorSt -> ProcessId -> Process MonitorSt
handleNormalTermination m@(MonitorSt{..}) pid = do
    case Map.lookup pid monWorkers of
      -- Unknown process
      Nothing          -> return m
      -- Single process terminated normally. Remove it from group
      Just (Left _)    -> return droppedPID
      -- Group
      Just (Right gid) -> case monGroups ! gid of
        -- Normal process group. Remove it from list of known process
        -- and remove group if all processes terminated.
        Group ps mch -> case Set.delete pid ps of
          ps' | Set.null ps' -> return droppedPID { monGroups = Map.delete gid monGroups }
              | otherwise    -> return droppedPID { monGroups = Map.insert gid (Group ps' mch) monGroups }
        -- Failout process group. Remove PID from list of known
        -- processes and send message if we had failures
        Failout ps mch n -> case Set.delete pid ps of
          ps' | Set.null ps' && n == 0 -> return droppedPID { monGroups = Map.delete gid monGroups }
              | Set.null ps'           -> case mch of
                  Just ch -> do sendChan ch (Just n)
                                return droppedPID { monGroups = Map.delete gid monGroups }
                  Nothing -> return droppedPID { monGroups = Map.insert gid (Failout ps' mch n) monGroups }
              | otherwise -> return droppedPID { monGroups = Map.insert gid (Failout ps' mch n) monGroups }
        -- Failed group. Normally we should never get here.
        FailedGroup -> return droppedPID
  where
    droppedPID = m { monWorkers = Map.delete pid monWorkers }

-- Handle crash of the process
handleCrash :: MonitorSt -> ProcessId -> Process MonitorSt
handleCrash m@(MonitorSt{..}) pid =
    case Map.lookup pid monWorkers of
      Nothing                  -> return m
      -- Single process. Send crash notification or update state
      Just (Left (Awaited ch)) -> do sendChan ch ()
                                     return droppedPID
      Just (Left _)            -> return m { monWorkers = Map.insert pid (Left Failed) monWorkers }
      -- Process group
      Just (Right gid) -> case monGroups ! gid of
        -- Normal process group. We terminate whole group on first error
        Group ps (Just ch) -> do sendChan ch Nothing
                                 T.forM_ ps $ \p -> kill p "Terminate group"
                                 return $ dropGID gid $ dropPIDS ps
        Group ps Nothing   -> do T.forM_ ps $ \p -> kill p "Terminate group"
                                 return (dropPIDS ps) { monGroups = Map.insert gid FailedGroup monGroups }
        -- Failout group. We keep number of failures.
        Failout ps mch n -> case Set.delete pid ps of
            ps' | Set.null ps' -> case mch of
                                    Just ch -> do sendChan ch (Just (n+1))
                                                  return $ droppedPID { monGroups = Map.delete gid monGroups }
                                    Nothing -> do return $ droppedPID { monGroups = Map.insert gid (Failout ps' mch (n+1)) monGroups }
                | otherwise    -> return $ droppedPID { monGroups = Map.insert gid (Failout ps' mch (n+1)) monGroups }
        -- Shouldn't really happen here
        FailedGroup -> return droppedPID
  where
    droppedPID  = m { monWorkers = Map.delete pid monWorkers }
    dropPIDS ps = m { monWorkers = T.foldr Map.delete monWorkers ps }
    dropGID gid mm = mm { monGroups = Map.delete gid monGroups }

-- Handle ask for the process state
handleAskProcess :: MonitorSt -> AskProcess -> Process MonitorSt
handleAskProcess st@(MonitorSt{..}) (AskProcess pid ch) = do
    case Map.lookup pid monWorkers of
      -- We don't know about process. So we assume it terminated normally.
      Nothing -> return st
      -- Single process
      Just (Left Awaited{}) -> error "SHOULD NOT HAPPEN"
      Just (Left Failed)    -> do sendChan ch ()
                                  return $ st { monWorkers = Map.delete pid monWorkers }
      Just (Left Running)   -> return $ st { monWorkers = Map.insert pid (Left (Awaited ch)) monWorkers }
      Just (Right _)        -> error "SHOULD NOT HAPPEN"


-- Handle ask for the process state
handleAskGroup :: MonitorSt -> AskGroup -> Process MonitorSt
handleAskGroup st@(MonitorSt{..}) (AskGroup gid ch) = do
    case Map.lookup gid monGroups of
      -- Assume group terminated normally
      Nothing -> return st
      -- Execution failed
      Just FailedGroup -> do sendChan ch Nothing
                             return $ st { monGroups = Map.delete gid monGroups }
      -- Normal group
      Just (Group pids _)
          | Set.null pids -> return $ st { monGroups = Map.delete gid monGroups }
          | otherwise     -> return $ st { monGroups = Map.insert gid (Group pids (Just ch)) monGroups }
      -- Failout group
      Just (Failout pids _ n)
          | Set.null pids && n == 0 -> return $ st { monGroups = Map.delete gid monGroups }
          | Set.null pids           -> do sendChan ch (Just n)
                                          return $ st { monGroups = Map.delete gid monGroups }
          | otherwise               -> return $ st { monGroups = Map.insert gid (Failout pids (Just ch) n) monGroups }

-- Handle registration of single process
handleRegisterProc :: MonitorSt -> RegisterPID -> Process MonitorSt
handleRegisterProc st@(MonitorSt{..}) (RegisterPID pid) = do
    return $ st { monWorkers = Map.insert pid (Left Running) monWorkers }

-- Handle registraction of normal group of processes
handleRegisterGroup :: (Int,MonitorSt) -> RegisterGroup -> Process (Int,MonitorSt)
handleRegisterGroup (n,st@MonitorSt{..}) (RegisterGroup pid group) = do
    let gid = GroupID n
    send pid gid
    return $ (n+1, st { monWorkers = foldr (.) id [Map.insert pid (Right gid) | pid <- group] monWorkers
                      , monGroups  = Map.insert gid (Group (Set.fromList group) Nothing) monGroups
                      })

-- Handle registraction of group of processes which uses failout
handleRegisterFailout :: (Int,MonitorSt) -> RegisterFailout -> Process (Int,MonitorSt)
handleRegisterFailout (n,st@MonitorSt{..}) (RegisterFailout pid group) = do
    let gid = GroupID n
    send pid gid
    return $ (n+1, st { monWorkers = foldr (.) id [Map.insert pid (Right gid) | pid <- group] monWorkers
                      , monGroups  = Map.insert gid (Failout (Set.fromList group) Nothing 0) monGroups
                      })


iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = loop a
   where
     loop = f >=> loop

----------------------------------------------------------------
-- Mapping between group
----------------------------------------------------------------

-- Mapping a <=> {b}
data GroupMap a b = GroupMap (Map a (Set b)) (Map b a)

lookupGID :: (Ord b) => b -> GroupMap a b -> Maybe a
lookupGID b (GroupMap _ idx) = Map.lookup b idx

-- | Delete member of group. If group is bein removed returns group ID
deleteMember :: (Ord a, Ord b)
             => b -> GroupMap a b -> (Maybe a, GroupMap a b)
deleteMember b m@(GroupMap groups index) =
    case Map.lookup b index of
      Nothing -> (Nothing,m)
      Just a  -> case Set.delete b (groups ! a) of
        bs | Set.null bs -> (Just a,  GroupMap (Map.delete a groups)    index')
           | otherwise   -> (Nothing, GroupMap (Map.insert a bs groups) index')
  where
    index' = Map.delete b index

deleteGroup :: (Ord a, Ord b) => a -> GroupMap a b -> GroupMap a b
deleteGroup a m@(GroupMap groups index) =
    case Map.lookup a groups of
      Nothing -> m
      Just bs -> GroupMap (Map.delete a groups) (T.foldr Map.delete index bs)

addGroup :: (Ord a, Ord b) => a -> [b] -> GroupMap a b -> GroupMap a b
addGroup a bs (GroupMap groups index) =
    GroupMap (Map.insert a (Set.fromList bs) groups)
             (T.foldr (\b -> Map.insert b a) index bs)
