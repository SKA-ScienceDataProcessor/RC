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
handleNormalTermination m@MonitorSt{..} pid = flip execStateT m $ 
    case Map.lookup pid monWorkers of
      -- Unknown process
      Nothing          -> return ()
      -- Single process terminated normally. Remove it from group
      Just (Left _)    -> dropPID pid
      -- Process from grooup terminated
      Just (Right gid) -> do mg <- lift $ processTerminated pid $ monGroups ! gid
                             updateGroup gid mg
                             dropPID pid

-- Handle crash of the process
handleCrash :: MonitorSt -> ProcessId -> Process MonitorSt
handleCrash m@(MonitorSt{..}) pid = flip execStateT m $ 
    case Map.lookup pid monWorkers of
      Nothing                  -> return ()
      -- Single process. Send crash notification or update state
      Just (Left (Awaited ch)) -> do lift $ sendChan ch ()
                                     dropPID pid
      Just (Left _)            -> do setPID pid Failed
      -- Process group
      Just (Right gid) -> do processCrashed pid gid $ monGroups ! gid
                             dropPID pid

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
      Just (CompletedGroup s) -> do sendChan ch s
                                    return $ st { monGroups = Map.delete gid monGroups }
      -- Normal group
      Just (Group pids _) -> return $ st { monGroups = Map.insert gid (Group pids (Just ch)) monGroups }
      -- Failout group
      Just (Failout pids _ n) -> return $ st { monGroups = Map.insert gid (Failout pids (Just ch) n) monGroups }

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


dropPID :: Monad m => ProcessId -> StateT MonitorSt m ()
dropPID pid = modify $ \m@MonitorSt{..} ->
    m { monWorkers = Map.delete pid monWorkers }

setPID :: Monad m => ProcessId -> ProcState -> StateT MonitorSt m ()
setPID pid s = modify $ \m@MonitorSt{..} ->
    m { monWorkers = Map.insert pid (Left s) monWorkers }

dropPIDs :: Monad m => Set ProcessId -> StateT MonitorSt m ()
dropPIDs pids = modify $ \m@MonitorSt{..} ->
    m { monWorkers = T.foldr Map.delete monWorkers pids }

updateGroup :: Monad m => GroupID -> Maybe GroupState -> StateT MonitorSt  m ()
updateGroup gid mst = modify $ \m@MonitorSt{..} ->
    case mst of
      Nothing -> m { monGroups = Map.delete gid    monGroups }
      Just st -> m { monGroups = Map.insert gid st monGroups }

dropGroup :: Monad m => GroupID -> StateT MonitorSt m ()
dropGroup gid = modify $ \m@MonitorSt{..} ->
    m { monGroups = Map.delete gid   monGroups }

setGroup :: Monad m => GroupID -> GroupState -> StateT MonitorSt m ()
setGroup gid st = modify $ \m@MonitorSt{..} ->
    m { monGroups = Map.insert gid st monGroups }

processTerminated :: ProcessId -> GroupState -> Process (Maybe GroupState)
processTerminated pid g = case g of
    Group ps mch -> case Set.delete pid ps of
      ps' | Set.null ps' -> return Nothing
          | otherwise    -> return $ Just $ Group ps' mch
    Failout ps mch n -> case Set.delete pid ps of
      ps' | Set.null ps' && n == 0 -> return Nothing
          | Set.null ps'           -> case mch of
              Just ch -> do sendChan ch (Just n)
                            return Nothing
              Nothing -> return $ Just $ CompletedGroup (Just n)
          | otherwise              -> return $ Just $ Failout ps' mch n
    CompletedGroup _ -> return $ Just g

processCrashed :: ProcessId -> GroupID -> GroupState -> StateT MonitorSt Process ()
processCrashed pid gid g = case g of
    Group ps mch -> do
        dropPIDs ps
        lift $ T.forM_ ps $ \p -> kill p "Terminate group"
        case mch of
          Just ch -> do lift $ sendChan ch Nothing
                        dropGroup gid
          Nothing -> setGroup gid $ CompletedGroup Nothing
    Failout ps mch n -> case Set.delete pid ps of
        ps' | Set.null ps' -> case mch of
                Just ch -> do lift $ sendChan ch (Just $ n + 1)
                              dropGroup gid
                Nothing -> setGroup gid $ CompletedGroup (Just (n+1))
            | otherwise -> setGroup gid $ Failout ps' mch (n+1)
    CompletedGroup _ -> return ()


-- Iterate monadict action indefinitelys
iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = loop a
   where
     loop = f >=> loop
