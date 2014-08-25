{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Library functions for the
module DNA.CH (
    -- * Array functions
    zipArray
  , foldArray
  , generateArrayShape
  , generateArraySlice
  , scatterShape
    -- * CH utils
  , Result(..)
  , RemoteMap(..)
  , sendToI
  , sendResult
  , handleRule
  , producer
  , startActor
  , defaultMain
  , monitorActors
  , scatterGather
  , worker
  , spawnActor
    -- * Reexports
  , Shape(..)
  , Slice(..)
    -- * Logging
  , startLogger
  , sayMsg
  , startTracing
  ) where

import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process.Platform.ManagedProcess
import Control.Distributed.Process.Platform.Time (Delay(..))
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)
import qualified Control.Distributed.Process.Debug as D
import qualified Control.Distributed.Process.Platform.Service.SystemLog as Log

import Data.Typeable (Typeable)
import Data.Binary   (Binary)
import qualified Data.Vector.Storable as S
import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq
import           Data.Sequence   (ViewL(..),(|>))
import System.Environment (getArgs)
import Text.Printf

import GHC.Generics (Generic)



newtype Shape = Shape Int
                deriving (Show,Eq,Typeable,Generic)
instance Binary Shape

data Slice = Slice Int Int
           deriving (Show,Eq,Typeable,Generic)
instance Binary Slice

data Array sh a = Array sh (S.Vector a)


----------------------------------------------------------------
-- Array manipulations
----------------------------------------------------------------

zipArray :: (S.Storable a, S.Storable b, S.Storable c, Eq sh)
         => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipArray f (Array shA va) (Array shB vb)
  | shA /= shB = error "Bad vector shape"
  | otherwise  = Array shA (S.zipWith f va vb)

foldArray :: (S.Storable a)
          => (a -> a -> a) -> a -> (Array sh a) -> a
foldArray f x0 (Array _ v) = S.foldl' f x0 v

generateArrayShape
  :: (S.Storable a) => Shape -> (Int -> a) -> Array Shape a
generateArrayShape sh@(Shape n) f = Array sh (S.generate n f)

generateArraySlice
  :: (S.Storable a) => Slice -> (Int -> a) -> Array Slice a
generateArraySlice sh@(Slice off n) f
  = Array sh (S.generate n (\i -> f (i + off)))

scatterShape :: Int -> Shape -> [Slice]
scatterShape n (Shape size)
  = zipWith Slice chunkOffs chunkSizes
  where
    (chunk,rest) = size `divMod` n
    extra        = replicate rest 1 ++ repeat 0
    chunkSizes   = zipWith (+) (replicate n chunk) extra
    chunkOffs    = scanl (+) 0 chunkSizes



----------------------------------------------------------------
-- CH combinators
----------------------------------------------------------------

newtype Result a = Result a
                   deriving (Eq,Ord,Show,Typeable,Binary)

data RemoteMap = RemoteMap
  { rmapResult :: ProcessId
  , rmapConns  :: IntMap.IntMap ProcessId
  }
  deriving (Typeable,Show,Generic)
instance Binary RemoteMap

-- | Send value to remote process
sendToI :: Serializable a => RemoteMap -> Int -> a -> Process ()
sendToI (RemoteMap _ m) i a = cast (m IntMap.! i) a

-- | Send result of computation to master process
sendResult :: Serializable a => RemoteMap -> a -> Process ()
sendResult (RemoteMap p _) a = send p (Result a)

-- | Handle for single transition rule for state machine
handleRule :: (Serializable a) => (s -> a -> Process (s,())) -> Dispatcher s
handleRule f
  = handleCast
  $ \s a -> do (s',()) <- f s a
               return (ProcessContinue s')

-- | Helper for starting state machine actor
startActor :: Process s -> ProcessDefinition s -> Process ()
startActor ms def = do
  s <- ms
  serve () (\_ -> return (InitOk s NoDelay)) def

-- | Helper for starting data producer actor
producer :: Process s -> (s -> Process (s,())) -> Process ()
producer s0 step = loop =<< s0
  where
    loop s = do (s',()) <- step s
                loop s'

-- | Default main
defaultMain :: (RemoteTable -> RemoteTable) -> ([NodeId] -> Process ()) -> IO ()
defaultMain remotes master = do
  args <- getArgs
  let rtable = remotes initRemoteTable
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable
      startMaster backend $ \nids -> do me <- getSelfPid
                                        master (processNodeId me : nids)
    ["slave", host, port] -> do
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> do putStrLn $ unlines
              [ "usage: '"++executableName++" (master|slave) host port"
              , "   or: '"++executableName++" write-tests"
              , ""
              , "'"++executableName++" write-tests' will write file 'start-ddp' into current directory."
              , "make it executable and run to test the program."
              ]
  where
    executableName = "EXE"

spawnActor :: NodeId -> Closure (Process ()) -> Process ProcessId
spawnActor nid clos = do
  (pid,_) <- spawnSupervised nid clos
  return pid

-- | Set up monitoring of slave processes
monitorActors :: Process ()
monitorActors = loop
  where
    loop = receiveWait
             [ match $ \(Result x) -> say $ "RESULT = " ++ show (x :: Double)
             , match $ \(Result x) -> say $ "RESULT = " ++ show (x :: Int)
             , match $ \(ProcessMonitorNotification _ pid reason) ->
                say $ printf "Process %s down: %s" (show pid) (show reason)
             ]



----------------------------------------------------------------
-- Scatter-gather
----------------------------------------------------------------

-- | Process for doing scatter\/gather.
scatterGather
  :: (Serializable a, Serializable b, Serializable c)
  => (Process c, c -> c -> Process c, c -> Process ())
     -- ^ Functions for doing gather
  -> (Int -> a -> Process [b])
     -- ^ Function for doing scatter
  -> Process ()
scatterGather (monadX0,merge,sendRes) scatter = do
  x0 <- monadX0
  -- Get necessary parameters
  workerNodes <- expect :: Process [NodeId]
  workerProc  <- expect
  let nWorker = length workerNodes
  workers <- forM workerNodes $ \nid -> spawn nid workerProc
  forM_ workers $ \pid -> send pid =<< getSelfPid
  -- Handler for gather message
  let handleGather (SGState _ NoAcc) _ = error "Internal error!"
      handleGather (SGState queue (Await acc n)) (Gather c) = do
        acc' <- merge acc c
        case n of
          1 -> do sendRes acc'
                  case Seq.viewl queue of
                    EmptyL  -> return $ ProcessContinue $ SGState queue NoAcc
                    a :< as -> do bs <- scatter nWorker a
                                  forM_ (zip bs workers) $ \(b,pid) -> cast pid (Scatter b)
                                  return $ ProcessContinue $ SGState as (Await x0 nWorker)
          _ -> return $ ProcessContinue $ SGState queue (Await acc' (n-1))
  -- Handler for scatter messages
  let handleScatter (SGState queue NoAcc) a = do
        bs <- scatter nWorker a
        forM_ (zip bs workers) $ \(b,pid) -> cast pid (Scatter b)
        return $ ProcessContinue $ SGState queue (Await x0 nWorker)
      handleScatter (SGState queue acc) a = do
        return $ ProcessContinue $ SGState (queue |> a) acc
  -- Definition of server
  startActor (return $ SGState Seq.empty NoAcc) $ defaultProcess
    { apiHandlers =
         [ handleCast handleGather
         , handleCast handleScatter
         ]
    }

-- | Function for worker
worker :: (Serializable a, Serializable b) => (a -> Process b) -> Process ()
worker f = do
  master <- expect :: Process ProcessId
  startActor (return ()) $ defaultProcess
    { apiHandlers = [ handleCast $ \() (Scatter a) -> do
                         b <- f a
                         cast master (Gather b)
                         return $ ProcessContinue ()
                    ]
    }


newtype Scatter a = Scatter a
                   deriving (Eq,Ord,Show,Typeable,Binary)
newtype Gather a = Gather a
                   deriving (Eq,Ord,Show,Typeable,Binary)

-- | Scatter-gather state
data SGState a c = SGState (Seq.Seq a) (GatherAcc c)

-- | Accumulator for gather
data GatherAcc a
  = NoAcc                       -- ^ Accumulator is empty
  | Await !a !Int               -- ^ Accumulator holding value and we expect n more answers



----------------------------------------------------------------
-- Logging
----------------------------------------------------------------

-- | Start the logger. Run it on master node.
startLogger :: [NodeId]         -- ^ List of all known nodes
            -> Process ()
startLogger peers = do
  pid <- getSelfPid
  _   <- spawnLocal $ loggerProcess peers pid
  -- Wait for logger process to initialize
  () <- expect
  return ()

-- Logger process - register itself and waits for Log messages.
loggerProcess :: [NodeId] -> ProcessId -> Process ()
loggerProcess peers starter = do
  me <- getSelfPid
  register loggerProcessName me
  forM_ peers $ \n ->
    registerRemoteAsync n loggerProcessName me
  send starter ()
  forever $ do
    LogMsg pid s <- expect
    liftIO $ printf "[%s] %s\n" (show pid) s


-- Common logger process name.
loggerProcessName :: String
loggerProcessName = "dna-logger"

-- |Send log messages to logger process.
sayMsg :: String -> Process ()
sayMsg msg = do
  logger <- Log.client
  case logger of
    Nothing -> do
      logg <- whereis loggerProcessName
      case logg of
        Just l -> do me <- getSelfPid
                     send l (LogMsg me msg)
	Nothing -> error "completely unable to resolve any logging facility!"
    Just cl -> do
      Log.debug cl (Log.LogText msg)

-- |Message to logger.
data LogMsg = LogMsg ProcessId String
         deriving (Show,Typeable,Generic)
instance Binary LogMsg where

-- | Enable tracing after all is set up.
startTracing :: [NodeId] -> Process ()
startTracing peers = do
  forM_ peers $ \peer -> do
    _ <- D.startTraceRelay peer
    D.setTraceFlags $ D.TraceFlags
       { D.traceSpawned      = Nothing
       , D.traceDied         = Nothing
       , D.traceRegistered   = Nothing
       , D.traceUnregistered = Nothing
       , D.traceSend         = Nothing -- Just D.TraceAll
       , D.traceRecv         = Nothing -- Just D.TraceAll
       , D.traceNodes        = True
       , D.traceConnections  = True
       }
    D.startTracer (liftIO . putStrLn . show)
