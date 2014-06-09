{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Very simple scheduler and node monitor
module Main where


import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Serializable (Serializable)

import qualified Data.Foldable as T
import Data.Binary
import Data.Typeable
import Data.Time.Clock
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map (Map)
import Text.Printf
import GHC.Generics (Generic)


----------------------------------------------------------------
-- Workers
----------------------------------------------------------------
newtype Result   a = Result   a deriving (Show,Typeable,Binary)
newtype BoundedV a = BoundedV a
newtype Count    a = Count    a deriving (Show,Typeable,Binary)
data X

newtype Idle = Idle ProcessId
             deriving (Show,Typeable,Generic)
instance Binary Idle


dotProductWorker :: (ProcessId,ProcessId) -> Process ()
dotProductWorker (pidMaster,pidUpstream) = do
  myPid <- getSelfPid
  send pidMaster (Idle myPid)
  loop
  where
    loop = do
      msg <- expect :: Process (Maybe (Int,Int))
      case msg of
        Nothing    -> return ()
        Just (i,j) -> do liftIO $ threadDelay (1000*1000)
                         send pidUpstream (1 :: Double)
                         myPid <- getSelfPid
                         send pidMaster (Idle myPid)
                         loop

foldWorker :: (SendPort String,ProcessId) -> Process ()
foldWorker (logCh,pid)
  = loop Nothing 0 0
  where
    loop :: Maybe Int -> Int -> Double -> Process ()
    loop (Just tot) !n !x
      | n >= tot = send pid (Result x)
    loop tot n x = do
      msg <- receiveWait [ match $ return . Val
                         , match $ return . Total . (\(Count x) -> x)
                         ]
      sendChan logCh $ show (msg,tot,n,x)
      case msg of
        Val   y -> loop tot (n+1) (x+y)
        Total k | n >= k    -> send pid (Result x)
                | otherwise -> loop (Just k) n x



data FoldTypes
  = Val   Double
  | Total Int
    deriving Show

remotable [ 'dotProductWorker
          , 'foldWorker
          ]


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

data BoundedProtocol a = BoundedProtocol ProcessId


data Chan a b where
  Noop :: Chan a b
  -- Sum bounded number of values
  FoldSum    :: ProcessId -> Chan (BoundedV a) (Result b)
  -- Primitive for calculation of dot product
  DotProduct
    :: Set ProcessId        -- Set of running processes
    -> BoundedProtocol a    -- Protocol for communication with downstream
    -> !Int                 -- Number of completed work
    -> [(Int,Int)]          -- Set of work
    -> Chan X (BoundedV a)
  -- Reified Category
  Id      :: Chan a a
  Compose :: Chan a x -> Chan x b -> Chan a b




-- | Process completed its execution normally
processDone :: ProcessId -> Chan a b -> Process (Chan a b)
processDone _    Id           = return Id
processDone _    Noop         = return Noop
processDone pid (Compose a b) = Compose <$> processDone pid a <*> processDone pid b
processDone pid p@(FoldSum pidF)
  | pid == pidF = return Noop
  | otherwise   = return p
processDone pid p@(DotProduct pids prot@(BoundedProtocol upstream) n work)
    -- Check for invalid state
  | Set.null pids' && not (null work) =
      error "Internal error: we still have work and doesn't have workers!"
    -- All work is done
  | Set.null pids' && null work =
      case prot of BoundedProtocol pid -> send upstream (Count n) >> return Noop
    -- Or we drop pid
  | otherwise =
      return $ DotProduct pids' prot n work
  where
    pids' = Set.delete pid pids


-- | Process crashed
processDown :: ProcessId -> Chan a b -> ExceptT String Process (Chan a b)
processDown _    Id           = return Id
processDown pid (Compose a b) = Compose <$> processDown pid a <*> processDown pid b
-- Crash of fold process is fatal.
processDown pid p@(FoldSum pidF)
  | pid == pidF = throwE $ "Fold process (" ++ show pidF ++ ") crashed"
  | otherwise   = return p
-- We simply discard failed processes and continue.
--
-- RACE: Process may crash between sending result to folder and
--       receiving additional work but we assume that we lose work in
--       any case.
processDown pid (DotProduct pids prot n work) =
  return $ DotProduct (Set.delete pid pids) prot (n - 1) work

-- | Process asking for more work
moreWork :: ProcessId -> Chan a b -> Process (Chan a b)
moreWork _    Id           = return Id
moreWork _    Noop         = return Noop
moreWork pid (Compose a b) = Compose <$> moreWork pid a <*> moreWork pid b
moreWork pid p@(FoldSum pidF)
  | pid == pidF = error $ "Fold cannot ask for more work. PID: " ++ show pidF
  | otherwise   = return p
moreWork pid p@(DotProduct pids prot n work)
  | pid `Set.member` pids = do
      case work of
        []   -> do send pid (Nothing :: Maybe (Int,Int))
                   return $ DotProduct pids prot n work
        w:ws -> do send pid $ Just w
                   return $ DotProduct pids prot (n+1) ws
  | otherwise = return p



----------------------------------------------------------------
-- Scheduler
----------------------------------------------------------------

-- | Scheduler which is performing
data Scheduler a = Scheduler
  { schedNodes :: Map NodeId [ProcessId]
  , process    :: Chan X (Result a)
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
schedStep :: SendPort String -> SchedMessage a -> Scheduler a -> Process (Outcome a (Scheduler a))
-- Monitored node crashed. We simply remove node from list of known
-- ones. Processes running on that nodes will be handled separately.
schedStep logCh (NodeDown (NodeMonitorNotification _ nid e)) sched = do
  sendChan logCh $ printf "* Node %s down: %s" (show nid) (show e)
  return $ Step $ sched { schedNodes = Map.delete nid (schedNodes sched) }
-- Monitored process terminated normally
schedStep logCh (ProcDown (ProcessMonitorNotification _ pid DiedNormal)) sched = do
  sendChan logCh $ printf "process %s completed" (show pid)
  let nid   = processNodeId pid
      nodes = Map.adjust (filter (/= pid)) nid (schedNodes sched)
  chan <- processDone pid (process sched)
  return $ Step $ Scheduler nodes chan
-- Monitored process crashed either by itself or because node crashed.
schedStep logCh (ProcDown (ProcessMonitorNotification _ pid e)) sched = do
  sendChan logCh $ printf "* Process %s down: %s" (show pid) (show e)
  let nid   = processNodeId pid
      nodes = Map.adjust (filter (/= pid)) nid (schedNodes sched)
  mchan <- runExceptT $ processDown pid (process sched)
  case mchan of
    Left  err  -> return $ Failure err
    Right chan -> return $ Step $ Scheduler nodes chan
-- Process asks for more work
schedStep logCh (ProcIdle (Idle pid)) sched = do
  sendChan logCh $ printf "more work for %s" (show pid)
  ch <- moreWork pid (process sched)
  return $ Step $ sched { process = ch }
-- Undefined
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

mainLoop :: SendPort String -> [NodeId] -> Process Double
mainLoop _     []  = error "Not enough nodes"
mainLoop _     [_] = error "Not enough nodes"
mainLoop logCh (nid:nodes) = do
  -- Spawn fold thread
  myPid       <- getSelfPid
  (pidFold,_) <- spawnSupervised nid $ $(mkClosure 'foldWorker) (logCh,myPid)
  -- Build workers
  let work = [(n*100,n+99) | n <- [0..10]]
  dotPids <- forM nodes $ \n -> do
    (pid,_) <- spawnSupervised n $ $(mkClosure 'dotProductWorker) (myPid,pidFold)
    return pid
  -- Assemble Chan
  let chan = (DotProduct (Set.fromList dotPids)
                         (BoundedProtocol pidFold)
                         0 work
             )
           `Compose`
             (FoldSum pidFold)
  -- Enter main loop
  loop $ Scheduler undefined chan
  where
    loop sched = do
      msg <- receiveWait matchSched
      res <- schedStep logCh msg sched
      case res of
        Failure   e -> error "TERMINATE"
        Completed a -> return a
        Step s      -> loop s

master :: Backend -> [NodeId] -> Process ()
master backend nodes = do
  logCh <- spawnChannelLocal logger
  x     <- mainLoop logCh nodes
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
  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable
