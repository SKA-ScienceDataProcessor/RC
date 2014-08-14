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
import Control.Distributed.Process hiding (say)
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
import System.IO

import Network.URI (URI(..), URIAuth(..), parseURI)

import DNA.Channel.File
import DNA.Message

import Common (startLogger, say)
import Cfg (executableName, event)

data PartialSum = PartialSum ProcessId Double deriving (Show,Typeable,Generic)
instance Binary PartialSum
newtype Result = Result Double deriving (Show,Typeable,Generic)
instance Binary Result


-- XXX should be an argument of the program
filePath :: FilePath
filePath="float_file.txt"

-- Collects data from compute processes, looking for either partial sum result or monitoring messages.
-- Partial sums are get accumulated.
dpSum :: [ProcessId] -> Double -> Process(Double)
dpSum [ ] sum = do
        return sum
dpSum pids sum = do
	receiveWait
		[ match $ \(PartialSum pid s) -> dpSum (filter (/= pid) pids) (s+sum)
		, match $ \(ProcessMonitorNotification _ pid _) -> dpSum (filter (/= pid) pids) sum
		]

spawnCollector :: ProcessId -> Process ()
spawnCollector pid = do
  	collectorPID <- getSelfPid
        masterPID <- dnaSlaveHandleStart "collector" collectorPID
        (DnaPidList computePids) <- expect

-- XXX The specification said the master monitors the compute nodes (failure of which is ignored)
-- XXX and the master monitors the collector (failure of which terminates the program)
	-- install monitors for compute processes.
	forM_ computePids $ \pid -> monitor pid
        sum <- event "collection phase" $ dpSum computePids 0
        send masterPID (Result sum)
	traceMessage "trace message from collector."

data FileVec = FileVec ProcessId (S.Vector Double) deriving (Eq, Show, Typeable, Generic)
instance Binary FileVec where
	put (FileVec pid vec) = put pid >> put vec
	get = do { pid <- get; vec <- get; return (FileVec pid vec)}


spawnFChan :: String -> Int -> Int -> ProcessId -> Process()
spawnFChan path cO cS pid = do
        mypid <- getSelfPid
	iov <- event "reading file" $ liftIO $ readData cS cO path
-- XXX must be an unsafe sending of the POINTER ONLY to avoid all data in the vector being touched.  Is that so?
        send pid (FileVec mypid iov)

instance (S.Storable e, Binary e) => Binary (S.Vector e) where
	put vec = put (S.toList vec)
	get = get >>= (return . S.fromList)


data CompVec = CompVec ProcessId (S.Vector Double) deriving (Eq, Show, Typeable, Generic)
instance Binary CompVec where
	put (CompVec pid vec) = put pid >> put vec
	get = do { pid <- get; vec <- get; return (CompVec pid vec)}

spawnCChan :: Int -> (Int -> Double) -> ProcessId -> Process()
spawnCChan n f pid = do
        myPid <- getSelfPid 
        let vec = S.generate n f
-- XXX must be an unsafe send to avoid copying
        event "generating and sending precomputed vector" $ send pid (CompVec myPid vec)


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

	--sayDebug $ printf "[Compute %s] : Value of iov: %s" (show computePID) (show iov) 

	let sumOnComputeNode = S.sum $ S.zipWith (*) iov cv
	sayDebug $ printf "[Compute] : sumOnComputeNode : %s at %s send to %s" (show sumOnComputeNode) (show computePID) (show collectorPID)
	event "compute sends sum" $ do
		send collectorPID (PartialSum computePID sumOnComputeNode)
        send masterPID (DnaFinished computePID)

remotable [ 'spawnCompute, 'spawnCollector]

master :: Backend -> [NodeId] -> Process ()
master backend peers = do
	startLogger peers
-- XXX Why is this comment here?
  --systemLog :: (String -> Process ()) -- ^ This expression does the actual logging
  --        -> (Process ())  -- ^ An expression used to clean up any residual state
  --        -> LogLevel      -- ^ The initial 'LogLevel' to use
  --        -> LogFormat     -- ^ An expression used to format logging messages/text
  --        -> Process ProcessId
        logPID <- Log.systemLog (liftIO . putStrLn) (return ()) Log.Debug return

	--startTracer $ \ev -> say $ "event: "++show ev

--	startTracer $ \ev -> do
--		sayDebug $ "evant in tracer: "++show ev

	Timer.sleep (Time.milliSeconds 100)

	masterPID <- getSelfPid
 	say $ printf "[Master %s]" (show masterPID)

-- XXX with a function please - this is messy
	-- enable tracing after all is set up.
--	forM_ peers $ \peer -> do
--		startTraceRelay peer
--		setTraceFlags $ TraceFlags {
--			  traceSpawned = Nothing
--			, traceDied = Nothing
--			, traceRegistered = Nothing
--			, traceUnregistered = Nothing
--			, traceSend = Just TraceAll
--			, traceRecv = Just TraceAll
--			, traceNodes = True
--			, traceConnections = True
--			}

--	enableTrace masterPID

--	traceMessage "trace message from master"

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

--	enableTrace collectorPid

        -- Start compute processes
  	computePids <- forM (zip3 allComputeNids chunkOffsets chunkSizes)  $ \(computeNid,chO,chS) -> do
  		pid <- dnaMasterStartSlave "compute" masterPID computeNid ( $(mkClosure 'spawnCompute) (filePath, chO, chS, itemCount, collectorPid)) 
		enableTrace pid
                return pid
	sum <- event "master waits for result" $ do

	        -- Send collector computePid's
        	send collectorPid (DnaPidList computePids)
	        sayDebug "--------------------------------------------------------------------------------------"	
        	(Result sum) <- expect
		return sum
        sayDebug $ printf "Result %s" (show sum)
  	terminateAllSlaves backend	


-- XXX Please make this the beginning of a DNA.Backend package
-- XXX not in this file
findSlavesByURIs :: [String] -> Int -> IO [NodeId]
findSlavesByURIs uris _ignoredTimeout = do
	nodes' <- mapM parseNode uris
	let errURIs = concatMap snd nodes'
	case errURIs of
		[] -> return $ concatMap fst nodes'
		errs -> do
			putStrLn $ "Error parsing uris: "++show errs
			hFlush stdout
			error "error parsing uris."
	where
		parseNode uri = do
			putStrLn $ "PArsing "++show uri
			hFlush stdout
			case parseURI uri of
				Just (URI _ (Just (URIAuth _ host port)) _ _ _) -> return (error uri, [])
				_ -> return ([], [uri])

main :: IO ()
main = do
	args <- getArgs

-- XXX This is now a real mess... Let's use an option package and get rid of all the hardcoded strings.
	case args of
 		["master", host, port] -> do
    			backend <- initializeBackend host port rtable
      			startMaster backend (master backend)
      			liftIO (threadDelay 200)

 		("master-nodes-uris" : host : port : uris) -> do
    			backend' <- initializeBackend host port rtable
			let backend = backend { findPeers = findSlavesByURIs uris }
      			startMaster backend (master backend)
      			liftIO (threadDelay 200)

		["slave", host, port] -> do
    			backend <- initializeBackend host port rtable
      			startSlave backend
		["write-tests"] -> do
			writeFile ("start-"++executableName) $ unlines [
				  "#!/bin/sh"
				, executableName++" slave localhost 60001 &"
				, executableName++" slave localhost 60002 &"
				, executableName++" slave localhost 60003 &"
				, "sleep 1"
				, executableName++" master localhost 44440"
				]
			writeFile ("start-"++executableName++"-uri") $ unlines [
				  "#!/bin/sh"
				, executableName++" slave localhost 60001 &"
				, executableName++" slave localhost 60002 &"
				, executableName++" slave localhost 60003 &"
				, "sleep 1"
				, executableName++" master-nodes-uris localhost 44440 slave://localhost:60001/ slave://127.0.0.1:60002/ slave://128.0.0.0:60003/"
				]
		["write-data", count] -> do
			return ()
                _ -> do putStrLn $ unlines [
				  "usage: '"++executableName++" (master|slave) host port"
				, "   or: '"++executableName++" master-nodes-uris host port ?uri ?uri?..?"
				, "   or: '"++executableName++" write-tests"
				, ""
				, "'"++executableName++" write-tests' will write files 'start-"++executableName ++"' and 'start-"++executableName++"-uri' into current directory."
				, "make them executable and run to test the program."
				]

  where
  	rtable :: RemoteTable
  	rtable = __remoteTable initRemoteTable
