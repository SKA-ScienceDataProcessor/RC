{-# LANGUAGE TemplateHaskell		#-}
{-# LANGUAGE DeriveDataTypeable		#-}
{-# LANGUAGE DeriveGeneric		#-}
{-# LANGUAGE GeneralizedNewtypeDeriving	#-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import GHC.Generics (Generic)
import Data.Typeable
import Control.Monad
import System.Posix.Files
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Platform (resolve)
import qualified Data.Vector.Storable as S
import Text.Printf
import Control.Distributed.Process.Debug
import qualified Control.Distributed.Process.Platform.Service.SystemLog as Log
import qualified Control.Distributed.Process.Platform.Time as Time
import qualified Control.Distributed.Process.Platform.Timer as Timer
import Data.Binary

import DNA.Channel.File
import DNA.Message

data PartialSum = PartialSum ProcessId Double deriving (Show,Typeable,Generic)
instance Binary PartialSum
newtype Result = Result Double deriving (Show,Typeable,Generic)
instance Binary Result

filePath :: FilePath
filePath="float_file.txt"

dpSum :: [ProcessId] -> Double -> Process(Double)
dpSum [ ] sum = do
      return sum
dpSum pids sum = do
      m <- expect
      case m of 
           (PartialSum pid s) -> dpSum (filter (/= pid) pids) (s+sum)
           _ -> say "Something wrong" >> terminate

spawnCollector :: ProcessId -> Process ()
spawnCollector pid = do
  	collectorPID <- getSelfPid
        masterPID <- dnaSlaveHandleStart "collector" collectorPID

        (DnaPidList computePids) <- expect
        sum <- dpSum computePids 0
        send masterPID (Result sum)
	traceMessage "trace message from collector."

data FileVec = FileVec ProcessId (S.Vector Double) deriving (Eq, Show, Typeable, Generic)

instance Binary FileVec where
	put (FileVec pid vec) = put pid >> put vec
	get = do { pid <- get; vec <- get; return (FileVec pid vec)}

-- XXX how do we make an S.Vector binary?
spawnFChan :: String -> Int -> Int -> ProcessId -> Process()
spawnFChan path cO cS pid = do
        mypid <- getSelfPid
	iov <- liftIO $ readData cS cO path
-- XXX must be an unsafe send to avoid copying
        send pid (FileVec mypid iov)

instance (S.Storable e, Binary e) => Binary (S.Vector e) where
	put vec = put (S.toList vec)
	get = get >>= (return . S.fromList)


data CompVec = CompVec ProcessId (S.Vector Double) deriving (Eq, Show, Typeable, Generic)
-- XXX how do we make an S.Vector binary?

instance Binary CompVec where
	put (CompVec pid vec) = put pid >> put vec
	get = do { pid <- get; vec <- get; return (CompVec pid vec)}

spawnCChan :: Int -> (Int -> Double) -> ProcessId -> Process()
spawnCChan n f pid = do
        myPid <- getSelfPid 
        let vec = S.generate n f
-- XXX must be an unsafe send to avoid copying
        send pid (CompVec myPid vec)


spawnCompute :: (FilePath, Int, Int, Int, ProcessId) -> Process ()
spawnCompute (file, chOffset, chSize, itemCount, collectorPID) = do
	getSelfPid >>= enableTrace
	computePID <- getSelfPid
	sayDebug $ printf "[Compute %s] : f:%s iC:%s cS:%s cO:%s coll:%s" (show computePID) file (show itemCount) (show chSize) (show chOffset) (show collectorPID)
        masterPID <- dnaSlaveHandleStart "compute" computePID

        fChanPid <- spawnLocal  (spawnFChan filePath chOffset chSize computePID)
        cChanPid <- spawnLocal  (spawnCChan chSize (\n -> 1.0) computePID)
        (FileVec fChanPid iov) <- expect
        (CompVec cChanPid cv) <- expect

	sayDebug $ printf "[Compute %s] : Value of iov: %s" (show computePID) (show iov) 

	let sumOnComputeNode = S.sum $ S.zipWith (*) iov cv
	sayDebug $ printf "[Compute] : sumOnComputeNode : %s at %s send to %s" (show sumOnComputeNode) (show computePID) (show collectorPID)
	send collectorPID (PartialSum computePID sumOnComputeNode)
        send masterPID (DnaFinished computePID)
	traceMessage "trace message from compute."

remotable [ 'spawnCompute, 'spawnCollector]

master :: Backend -> [NodeId] -> Process ()
master backend peers = do
  --systemLog :: (String -> Process ()) -- ^ This expression does the actual logging
  --        -> (Process ())  -- ^ An expression used to clean up any residual state
  --        -> LogLevel      -- ^ The initial 'LogLevel' to use
  --        -> LogFormat     -- ^ An expression used to format logging messages/text
  --        -> Process ProcessId
        logPID <- Log.systemLog (liftIO . putStrLn) (return ()) Log.Debug return

	startTracer $ \ev -> say $ "event: "++show ev

--	startTracer $ \ev -> do
--		sayDebug $ "evant in tracer: "++show ev

	Timer.sleep (Time.milliSeconds 100)

	masterPID <- getSelfPid
 	say $ printf "[Master %s]" (show masterPID)

	-- enable tracing after all is set up.
	forM_ peers $ \peer -> do
		startTraceRelay peer
		setTraceFlags $ TraceFlags {
			  traceSpawned = Nothing
			, traceDied = Nothing
			, traceRegistered = Nothing
			, traceUnregistered = Nothing
			, traceSend = Just TraceAll
			, traceRecv = Just TraceAll
			, traceNodes = True
			, traceConnections = True
			}

	enableTrace masterPID

	traceMessage "trace message from master"

-- Set up scheduling variables
  	let allComputeNids = tail peers
	let chunkCount = length allComputeNids 
	fileStatus <- liftIO $ getFileStatus filePath 
	let itemCount = div (read $ show (fileSize fileStatus)) itemSize
	liftIO . putStrLn $ "itemcount:  " ++  (show itemCount)

	let chunkOffsets = map (chunkOffset chunkCount itemCount) [1..chunkCount]
	liftIO . putStrLn $ "Offsets : " ++ show chunkOffsets
  	let chunkSizes = map (chunkSize chunkCount itemCount) [1..chunkCount]
  	liftIO . putStrLn $ "chunkSizes : " ++  show chunkSizes


	-- Start collector process
 	let collectorNid = head peers
        collectorPid <- dnaMasterStartSlave "collector" masterPID collectorNid ($(mkClosure 'spawnCollector) (masterPID))

	enableTrace collectorPid

        -- Start compute processes
  	computePids <- forM (zip3 allComputeNids chunkOffsets chunkSizes)  $ \(computeNid,chO,chS) -> do
  		pid <- dnaMasterStartSlave "compute" masterPID computeNid ( $(mkClosure 'spawnCompute) (filePath, chO, chS, itemCount, collectorPid)) 
		enableTrace pid
                return pid

        -- Send collector computePid's
        send collectorPid (DnaPidList computePids)
        sayDebug "--------------------------------------------------------------------------------------"	
        (Result sum) <- expect
        sayDebug $ printf "Result %s" (show sum)
  	terminateAllSlaves backend	

main :: IO ()
main = do
	args <- getArgs

	case args of
 		["master", host, port] -> do
    			backend <- initializeBackend host port rtable
      			startMaster backend (master backend)
      			liftIO (threadDelay 200)

		["slave", host, port] -> do
    			backend <- initializeBackend host port rtable
      			startSlave backend
		["write-tests"] -> do
			writeFile "start-ddp" $ unlines [
				  "#!/bin/sh"
				, "ddp-erlang-style slave localhost 60001 &"
				, "ddp-erlang-style slave localhost 60002 &"
				, "ddp-erlang-style slave localhost 60003 &"
				, "sleep 1"
				, "ddp-erlang-style master localhost 44440"
				]
		["write-data", count] -> do
			return ()
                _ -> do
			putStrLn "usage: 'ddp-erlang-style (master|slave) host port"
			putStrLn "   or: 'ddp-erlang-style write-tests"
			putStrLn ""
			putStrLn "'ddp-erlang-style write-tests' will write file 'start-ddp' into current directory."
			putStrLn "make it executable and run to test the program."

  where
  	rtable :: RemoteTable
  	rtable = __remoteTable initRemoteTable
