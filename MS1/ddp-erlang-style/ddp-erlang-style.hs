{-# LANGUAGE TemplateHaskell		#-}
{-# LANGUAGE DeriveDataTypeable		#-}
{-# LANGUAGE DeriveGeneric		#-}
{-# LANGUAGE GeneralizedNewtypeDeriving	#-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import GHC.Generics (Generic)
import Data.Typeable
import Control.DeepSeq
import Control.Monad
import System.Posix.Files
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process hiding (say)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Platform (resolve)
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe
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

import DNA.Common (startLogger, say, startTracing)

import Cfg (executableName, timePeriod, timePeriodPure, synchronizationPoint)

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
        synchronizationPoint
        collectorPID <- getSelfPid
        masterPID <- dnaSlaveHandleStart "collector" collectorPID
        (DnaPidList computePids) <- expect

-- XXX The specification said the master monitors the compute nodes (failure of which is ignored)
-- XXX and the master monitors the collector (failure of which terminates the program)
        -- install monitors for compute processes.
        forM_ computePids $ \pid -> monitor pid
        sum <- timePeriod "collection phase" $ dpSum computePids 0
        send masterPID (Result sum)
        traceMessage "trace message from collector."


data CompVec = CompVec ProcessId (S.Vector Double) deriving (Eq, Show, Typeable, Generic)

instance Binary CompVec where
        put (CompVec pid vec) = put pid >> put vec
        get = do { pid <- get; vec <- get; return (CompVec pid vec)}

instance NFData CompVec where
        rnf (CompVec p v) = rnf p `seq` rnf v

spawnCChan :: Int -> (Int -> Double) -> ProcessId -> Process()
spawnCChan n f pid = do
        myPid <- getSelfPid 
        let vec = S.generate n f
        timePeriod "generating and sending precomputed vector" $ Unsafe.send pid (CompVec myPid $! vec)


spawnCompute :: (FilePath, Int, Int, Int, ProcessId) -> Process ()
spawnCompute (file, chOffset, chSize, itemCount, collectorPID) = do
        synchronizationPoint
        getSelfPid >>= enableTrace
        computePID <- getSelfPid
        sayDebug $ printf "[Compute %s] : f:%s iC:%s cS:%s cO:%s coll:%s" (show computePID) file (show itemCount) (show chSize) (show chOffset) (show collectorPID)
        masterPID <- dnaSlaveHandleStart "compute" computePID

        fChanPid <- spawnLocal  (spawnFChan filePath chOffset chSize computePID)
        cChanPid <- spawnLocal  (spawnCChan chSize (\n -> 1.0) computePID)
        (iov, cv) <- timePeriod "receiving vectors" $ do
                (FileVec fChanPid iov) <- timePeriod "receiving read vector" expect
                (CompVec cChanPid cv) <- timePeriod "receiving computed vector" expect
                return (iov, cv)

        --sayDebug $ printf "[Compute %s] : Value of iov: %s" (show computePID) (show iov) 

        sumOnComputeNode <- timePeriod "compute sends sum" $ do
                let sumOnComputeNode = timePeriodPure "pure computation time" $ S.sum $ S.zipWith (*) iov cv
                send collectorPID (PartialSum computePID sumOnComputeNode)
                return sumOnComputeNode
        sayDebug $ printf "[Compute] : sumOnComputeNode : %s at %s send to %s" (show sumOnComputeNode) (show computePID) (show collectorPID)
        send masterPID (DnaFinished computePID)

remotable [ 'spawnCompute, 'spawnCollector]

-- |Monitoring of processes. We watch out for collector process, ignoring all other failures.
masterMonitor :: ProcessId -> Process Double
masterMonitor collectorPid = do
        -- if we will just return the value, we may stackoverflow on really big number of processes (1e6+).
        -- so we return a value to dispatch later.
        maybeResult <- receiveWait
                [ match $ \(ProcessMonitorNotification _ pid _) ->
                        if pid == collectorPid
                                then do
                                        sayDebug "Collector failure. Master process terminate."
                                        terminate
                                else return Nothing
                , match $ \(Result sum) -> return (Just sum)
                ]
        -- dispatch result.
        case maybeResult of
                Nothing -> masterMonitor collectorPid
                Just r -> return r

master :: Backend -> [NodeId] -> Process ()
master backend peers = do
        synchronizationPoint
        startLogger peers
        logPID <- Log.systemLog (liftIO . putStrLn) (return ()) Log.Debug return

        Timer.sleep (Time.milliSeconds 100)

        masterPID <- getSelfPid
        say $ printf "[Master %s]" (show masterPID)

        -- startTracing peers
        -- enableTrace masterPID

        -- traceMessage "trace message from master"

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

        -- enableTrace collectorPid

        -- Start compute processes
        computePids <- forM (zip3 allComputeNids chunkOffsets chunkSizes)  $ \(computeNid,chO,chS) -> do
                pid <- dnaMasterStartSlave "compute" masterPID computeNid ( $(mkClosure 'spawnCompute) (filePath, chO, chS, itemCount, collectorPid)) 
                enableTrace pid
                return pid
        sum <- timePeriod "master waits for result" $ do

                -- Send collector computePid's
                send collectorPid (DnaPidList computePids)
                sayDebug "--------------------------------------------------------------------------------------"
                masterMonitor collectorPid
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
