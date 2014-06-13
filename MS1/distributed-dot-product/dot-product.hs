{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Very simple scheduler and node monitor
module Main where

import System.Environment (getArgs)
import Control.Applicative
import Control.Monad
import Control.Category
import Control.Monad.Trans.Except
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Serializable (Serializable)

import Data.Binary
import Data.Typeable
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)
import Text.Printf
import GHC.Generics (Generic)
import Prelude hiding  ((.),id)

import Chan
import Worker
import Types


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------


-- | Process completed its execution normally
processDone :: ProcessId -> Chan Running a b -> Process (Chan Running a b)
processDone _    Id           = return Id
processDone _    Noop         = return Noop
processDone pid (Compose a b) = Compose <$> processDone pid a <*> processDone pid b
processDone pid p@(FoldSum (ActorFoldRunning pidF _))
  | pid == pidF = return Noop
  | otherwise   = return p
processDone pid p@(DotProduct (ActorDotProductRunning pids upstream n work))
    -- Check for invalid state
  | Set.null pids' && not (null work) =
      -- FIXME: it could happen if all workers die and we still have work.
      ---       In this case no progress is possible and we need to terminate.
      error "Internal error: we still have work and doesn't have workers!"
    -- All work is done
  | Set.null pids' && null work =
      sendBoundedCount upstream n >> return Noop
  | otherwise =
      return $ DotProduct (ActorDotProductRunning pids' upstream n work)
  where
    pids' = Set.delete pid pids


-- | Process crashed
processDown :: ProcessId -> Chan Running a b -> ExceptT String Process (Chan Running a b)
processDown _    Id           = return Id
processDown _    Noop         = return Noop
processDown pid (Compose a b) = Compose <$> processDown pid a <*> processDown pid b
-- Crash of fold process is fatal.
processDown pid p@(FoldSum (ActorFoldRunning pidF _))
  | pid == pidF = throwE $ "Fold process (" ++ show pidF ++ ") crashed"
  | otherwise   = return p
-- We simply discard failed processes and continue.
--
-- RACE: Process may crash between sending result to folder and
--       receiving additional work but we assume that we lose work in
--       any case.
processDown pid p@(DotProduct (ActorDotProductRunning pids upstream n work))
  | pid `Set.member` pids
      = return $ DotProduct $ ActorDotProductRunning (Set.delete pid pids) upstream (n - 1) work
  | otherwise = return p

-- | Process asking for more work
moreWork :: ProcessId -> Chan Running a b -> Process (Chan Running a b)
moreWork _    Id           = return Id
moreWork _    Noop         = return Noop
moreWork pid (Compose a b) = Compose <$> moreWork pid a <*> moreWork pid b
moreWork pid p@(FoldSum (ActorFoldRunning pidF _))
  | pid == pidF = error $ "Fold cannot ask for more work. PID: " ++ show pidF
  | otherwise   = return p
moreWork pid p@(DotProduct (ActorDotProductRunning pids prot n work))
  | pid `Set.member` pids = do
      case work of
        []   -> do send pid (Nothing :: Maybe (Int,Int))
                   return $ DotProduct $ ActorDotProductRunning pids prot n work
        w:ws -> do send pid $ Just w
                   return $ DotProduct $ ActorDotProductRunning pids prot (n+1) ws
  | otherwise = return p



----------------------------------------------------------------
-- Scheduler
----------------------------------------------------------------

-- | Scheduler which is performing
data Scheduler a = Scheduler
  { schedNodes :: Map NodeId [ProcessId]
  , process    :: Chan Running X (Result a)
  }

-- | Result of one scheduler step
data Outcome a b
  = Failure   String            -- ^ Irrecoverable failure. We must stop all running processes
  | Completed a                 -- ^ Task is completed
  | Step      b                 -- ^ Message is processed

data SchedMessage a
  = NodeDown    NodeMonitorNotification
  | ProcDown    ProcessMonitorNotification
  | ProcIdle    Idle
  | SchedResult a
  deriving (Show,Typeable,Generic)
instance Binary a => Binary (SchedMessage a)


-- | Single step of scheduler
schedStep :: MasterProtocol -> SchedMessage a -> Scheduler a -> Process (Outcome a (Scheduler a))
-- Monitored node crashed. We simply remove node from list of known
-- ones. Processes running on that nodes will be handled separately.
schedStep masterP (NodeDown (NodeMonitorNotification _ nid e)) sched = do
  logMsg masterP $ printf "MASTER: Node %s down: %s" (show nid) (show e)
  return $ Step $ sched { schedNodes = Map.delete nid (schedNodes sched) }
-- Monitored process terminated normally
schedStep masterP (ProcDown (ProcessMonitorNotification _ pid DiedNormal)) sched = do
  logMsg masterP $ printf "MASTER: process %s completed" (show pid)
  let nid   = processNodeId pid
      nodes = Map.adjust (filter (/= pid)) nid (schedNodes sched)
  chan <- processDone pid (process sched)
  return $ Step $ Scheduler nodes chan
-- Monitored process crashed either by itself or because node crashed.
schedStep masterP (ProcDown (ProcessMonitorNotification _ pid e)) sched = do
  logMsg masterP $ printf "MASTER: Process %s down: %s" (show pid) (show e)
  let nid   = processNodeId pid
      nodes = Map.adjust (filter (/= pid)) nid (schedNodes sched)
  mchan <- runExceptT $ processDown pid (process sched)
  case mchan of
    Left  err  -> return $ Failure err
    Right chan -> return $ Step $ Scheduler nodes chan
-- Process asks for more work
schedStep masterP (ProcIdle (Idle pid)) sched = do
  logMsg masterP $ printf "MASTER: more work for %s" (show pid)
  ch <- moreWork pid (process sched)
  return $ Step $ sched { process = ch }
schedStep _ (SchedResult a) _
  = return (Completed a)



matchSched :: (Serializable a) => [Match (SchedMessage a)]
matchSched =
  [ match $ return
  , match $ return . NodeDown
  , match $ return . ProcDown
  , match $ return . ProcIdle
  , match $ return . SchedResult . (\(Result x) -> x)
  ]

mainLoop :: MasterProtocol -> [NodeId] -> Process Double
mainLoop _     []  = error "Not enough nodes"
mainLoop _     [_] = error "Not enough nodes"
mainLoop masterP nodes = do
  let program = FoldSum    (ActorFoldWaiting $ \rP -> $(mkClosure 'foldWorker) (masterP,rP))
              . DotProduct (ActorDotProductWaiting (0,1000))
  chan <- startChan masterP nodes program
  loop $ Scheduler (Map.fromList [(n,[]) | n <- nodes]) chan
  where
    loop sched = do
      msg <- receiveWait matchSched
      res <- schedStep masterP msg sched
      case res of
        -- FIXME: terminate remaining processes
        Failure   e -> error "TERMINATE"
        Completed a -> return a
        Step s      -> loop s


master :: Backend -> [NodeId] -> Process ()
master backend nodes = do
  logCh <- spawnChannelLocal logger
  me    <- getSelfPid
  x     <- mainLoop (MasterProtocol logCh me) nodes
  sendChan logCh $ "RESULT: " ++ show x
  terminateAllSlaves backend

-- | Logger which prints messages on the local node
logger :: ReceivePort String -> Process ()
logger chan = forever $ liftIO . putStrLn =<< receiveChan chan


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> error "Unknown command"
  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable
