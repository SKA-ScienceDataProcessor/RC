{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric #-}
-- |
-- Process which monitor execution of all other processes.
--
-- We have to use dedicated process for monitoring. One alternative is
-- to let parent do monitoring but it doesnt' wokr for following
-- reasons:
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
import qualified Data.Map.Strict as Map
import           Data.Map.Strict   (Map)
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
    pid <- spawnLocal $ iterateM step (S 0 Map.empty Map.empty)
    return $ Monitor pid


-- Single step of monitor
step :: S -> Process S
step st = receiveWait
  [ match $ \(ProcessMonitorNotification _ pid reason) -> case reason of
                DiedUnknownId -> error "FIXME: not clear how to handle such case"
                DiedNormal    -> handleNormalTermination st pid
                _             -> handleCrash             st pid
  , match $ handleAskProcess      st
  , match $ handleAskGroup        st
  , match $ handleRegisterProc    st
  , match $ handleRegisterGroup   st
  , match $ handleRegisterFailout st
  ]


-- Handle normal termination of the process
handleNormalTermination :: S -> ProcessId -> Process S
handleNormalTermination st pid = do
    case Map.lookup pid (stWorkers st) of
      Nothing          -> error "SHOULD NOT HAPPEN"
      Just (Left _)    -> return $ withPID pid Nothing st
      Just (Right gid) -> withGID gid (dropGroupPID pid)
                        $ withPID pid Nothing st


-- Handle crash of process.
handleCrash :: S -> ProcessId -> Process S
handleCrash st pid = do
    case Map.lookup pid (stWorkers st) of
      Nothing -> error "SHOULD NOT HAPPEN"
      -- Single processes
      Just (Left (Awaited ch)) -> do sendChan ch ()
                                     return $ withPID pid Nothing st
      Just (Left _)  -> return $ withPID pid (Just (Left Failed)) st
      -- Process groups
      Just (Right gid) -> withGID gid (dropCrashedPID pid)
                        $ withPID pid Nothing st

-- Handle ask for the process state
handleAskProcess :: S -> AskProcess -> Process S
handleAskProcess st (AskProcess pid ch) = do
    case Map.lookup pid (stWorkers st) of
      Nothing -> return st
      Just (Left Awaited{}) -> error "SHOULD NOT HAPPEN"
      Just (Left Failed)    -> do sendChan ch ()
                                  return $ withPID pid Nothing st
      Just (Left  _)        -> return $ withPID pid (Just (Left (Awaited ch))) st
      Just (Right _)        -> error "SHOULD NOT HAPPEN"

-- Handle ask for the process state
handleAskGroup :: S -> AskGroup -> Process S
handleAskGroup st (AskGroup gid ch) = do
    case Map.lookup gid (stGroups st) of
      Nothing -> return st
      Just FailedGroup -> do sendChan ch Nothing
                             return $ st { stGroups = Map.delete gid (stGroups st) }
      Just (Group _ pids)
          | Set.null pids -> return $ st { stGroups = Map.delete gid (stGroups st) }
          | otherwise     -> return $ st { stGroups = Map.insert gid (Group (Just ch) pids) (stGroups st) }
      Just (Failout _ pids n)
          | Set.null pids && n == 0 -> return $ st { stGroups = Map.delete gid (stGroups st) }
          | Set.null pids           -> do sendChan ch (Just n)
                                          return $ st { stGroups = Map.delete gid (stGroups st) }
          | otherwise               -> return $ st { stGroups = Map.insert gid (Failout (Just ch) pids n) (stGroups st) }

-- Handle registration of single process
handleRegisterProc :: S -> RegisterPID -> Process S
handleRegisterProc st (RegisterPID pid) = do
    return $ st { stWorkers = Map.insert pid (Left Running) (stWorkers st) }

-- Handle registraction of normal group of processes
handleRegisterGroup :: S -> RegisterGroup -> Process S
handleRegisterGroup st (RegisterGroup pid group) = do
    let n   = stCounter st
        gid = GroupID n
    send pid gid
    return $ st
        { stCounter = n + 1
        , stWorkers = foldr (.) id [Map.insert pid (Right gid) | pid <- group]
                    $ stWorkers st
        , stGroups  = Map.insert gid (Group Nothing (Set.fromList group)) (stGroups st)
        }
    
-- Handle registraction of group of processes which uses failout
handleRegisterFailout :: S -> RegisterFailout -> Process S
handleRegisterFailout st (RegisterFailout pid group) = do
    let n   = stCounter st
        gid = GroupID n
    send pid gid
    return $ st
        { stCounter = n + 1
        , stWorkers = foldr (.) id [Map.insert pid (Right gid) | pid <- group]
                    $ stWorkers st
        , stGroups  = Map.insert gid (Failout Nothing (Set.fromList group) 0) (stGroups st)
        }


-- Update process ID in monitor state
withPID :: ProcessId -> Maybe (Either ProcState GroupID) -> S -> S
withPID pid a s =
    s { stWorkers = Map.update (const a) pid (stWorkers s)
      }

-- Update group state in monitor state
withGID :: Monad m
        => GroupID -> (GroupState -> m (Maybe GroupState)) -> S -> m S
withGID gid f st =
    case Map.lookup gid (stGroups st) of
      Nothing -> error "SHOULD NOT HAPPEN!"
      Just  g -> do
          r <- f g
          return $ st { stGroups = Map.update (\_ -> r) gid (stGroups st) }



-- Drop normally terminated PID from group
dropGroupPID :: ProcessId -> GroupState -> Process (Maybe GroupState)
dropGroupPID pid s = case s of
    -- Normal group
    Group mp pids -> return $ do pids' <- remove pids
                                 return $ Group mp pids
    -- Failout group
    Failout mp pids n -> case remove pids of
        Nothing | n == 0    -> return Nothing
                | otherwise ->
                      case mp of
                        Just ch -> do sendChan ch (Just n)
                                      return Nothing
                        Nothing -> return $ Just $ Failout mp Set.empty n
        Just pids' -> return $ Just $ Failout mp pids' n
    -- Failed group. Do nothing
    FailedGroup -> return (Just FailedGroup)
  where
    remove pids = case Set.delete pid pids of
                    x | Set.null x -> Nothing
                      | otherwise  -> Just x

-- Drop normally terminated PID from group
dropCrashedPID :: ProcessId -> GroupState -> Process (Maybe GroupState)
dropCrashedPID pid s = case s of
    -- Normal group
    Group mp pids -> do T.forM_ pids $ \pid -> (kill pid "DIE IN FILE")
                        return $ Just FailedGroup
    -- Failout group
    Failout mp pids n -> case remove pids of
        Nothing -> case mp of
                     Just ch -> do sendChan ch (Just (n+1))
                                   return Nothing
                     Nothing -> return $ Just $ Failout mp Set.empty (n+1)
        Just pids' -> return $ Just $ Failout mp pids' (n+1)
    -- Failed group. Do nothing
    FailedGroup -> return (Just FailedGroup)
  where
    remove pids = case Set.delete pid pids of
                    x | Set.null x -> Nothing
                      | otherwise  -> Just x


-- State of monitor. We keep here both
data S = S
  { stCounter :: !Int
    -- Counter for generation of fresh IDs
  , stWorkers :: !(Map ProcessId (Either ProcState GroupID))
    -- Map for processes. For single process we keep its state and for
    -- process in group their GID
  , stGroups  :: !(Map GroupID GroupState)
  }

-- State process group
data GroupState
    = Group   (Maybe (SendPort (Maybe Int))) (Set.Set ProcessId)
      -- Group of normal processes. 
    | Failout (Maybe (SendPort (Maybe Int))) (Set.Set ProcessId) Int
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

iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = loop a
  where
    loop = f >=> loop
