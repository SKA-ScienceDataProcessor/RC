{-# LANGUAGE TemplateHaskell		#-}
{-# LANGUAGE DeriveDataTypeable		#-}
{-# LANGUAGE DeriveGeneric		#-}
{-# LANGUAGE GeneralizedNewtypeDeriving	#-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Text.Printf
import Data.List
import Data.Binary
import Data.Typeable
import Control.Monad
import System.Posix.Files
import Foreign.Ptr
import Foreign.C.Types
import GHC.Generics (Generic)
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS

import DNAChannelFile 

newtype Message = Ping ProcessId deriving (Eq, Ord, Show, Binary, Typeable)

newtype PartialSum   = PartialSum Double deriving (Show,Typeable,Binary)

newtype FileMsg = FileMsg (S.Vector Double)  deriving (Show, Typeable)

data Start = FileChannelStart(SendPort Start) | StartFound String deriving (Show,Typeable, Binary)
--instance Binary Start 

filePath :: FilePath
filePath="float_file.txt"


--createFileReadChannel :: ProcessId -> Int -> Int -> String -> Process()
--createFileReadChannel pid chunkSize chunkOffset file = do 
createFileReadChannel :: ProcessId -> Process()
createFileReadChannel  = do 
	FileChannelStart sendPort <- expect 
	--liftIO $ putStrLn $ "Got"
	sendChan sendPort (StartFound "DNA") 
	
       -- (Ping pid) <- expect
        --send pid (Ping pid)


spawnCHOnCollector :: ([NodeId]) -> Process ()
spawnCHOnCollector (computeIds) = do


  	local_pid <- getSelfPid
	say $ printf "[Collector] : Waiting for master instruction!! : %s" (show local_pid)	

  	(Ping remote_pid) <- expect
  	say $ printf "[Collector] : Ping from master :%s" (show remote_pid)

  	send remote_pid (Ping local_pid)
  	say $ printf "[Collector] : Ready for further computation :%s" (show local_pid)

	forM_ computeIds  $ \computeId -> do
		--send computePid(Ping local_pid)	
               	say $ printf "[Collector-Compute]: Expecting RESULT from Compute Node %s" (show remote_pid)
               	(PartialSum localSum) <- expect
               	say $ printf "[Collector]: RESULT : %s from Compute Node : %s " (show localSum) (show computeId)
               	--say $ printf "[Colle`ctor]: RESULT :  from Compute Node : %s " (show computeId)

spawnCHOnCompute :: (ProcessId,FilePath, Int, Int) -> Process ()
spawnCHOnCompute (pid, file, chunkOffset, chunkSize) = do

	local_pid <- getSelfPid
	say $ printf "[Compute] : Waiting for master instruction!! :%s" (show local_pid)	

	(Ping remote_pid) <- expect
    	say $ printf "[Compute] : Ping from PID :%s on compute: %s" (show remote_pid) (show local_pid) 

	send remote_pid (Ping local_pid)
	say $ printf "[Compute] : Ready for further computation :%s" (show local_pid)

	say $ printf "[Compute] : Value of chunkOffset : [[%s]] at %s" (show chunkOffset) (show local_pid)
	say $ printf "[Compute] : Value of chunkSize : [[%s]] at %s" (show chunkSize) (show local_pid)

	fpid <- spawnLocal local_pid $ do 
		createFileReadChannel local_pid
		(sendPort,rcvPort) <- newChan
		send fpid(FileChannelStart sendPort)
		send fpid(FileChannelStart sendPort)
		StartFound cmf <- receiveChan rcvPort
		--print $ "found START to filechannel" ++  g
		liftIO $ putStrLn $ "Got : " ++ cmf	
		say "Start.................."  
	
	
	iov <- liftIO $ readData chunkSize chunkOffset file 
	say $ printf "[Compute] : Value of iov: %s at %s" (show iov) (show local_pid)		
		
	let sumOnComputeNode = S.sum iov
	say $ printf "[Compute] : sumOnComputeNode : %s at %s" (show sumOnComputeNode) (show local_pid)

	send pid (PartialSum sumOnComputeNode) 
	say $ printf "[Compute] : Sending results to Controller : %s" (show pid)


remotable [ 'spawnCHOnCompute, 'spawnCHOnCollector ]

master :: Backend -> [NodeId] -> Process ()
master backend peers = do

	local_pid <- getSelfPid
 	say $ printf "[Master] : Master Initiated !! :%s" (show local_pid)

	-- Reading first NodeID for controller
 	let controllerNid = head peers
  	let allComputePids = tail peers
 	say $ printf "[Master] : Spawning CH process on Collector Node :%s" (show controllerNid)
 	controllerPid <- spawn controllerNid ( $(mkClosure 'spawnCHOnCollector) (allComputePids))
	send controllerPid(Ping local_pid)

	say "--------------------------------------------------------------------------------------"	

	let chunkCount = length allComputePids 
	fileStatus <- liftIO $ getFileStatus filePath 
	let itemCount = div (read $ show (fileSize fileStatus)) itemSize
	liftIO . putStrLn $ "itemcount:  " ++  (show itemCount)
	
	let chunkOffsets = map (chunkOffset chunkCount itemCount) [1..chunkCount]
	liftIO . putStrLn $ "Offsets : " ++ show chunkOffsets
  	let chunkSizes = map (chunkSize chunkCount itemCount) [1..chunkCount]
  	liftIO . putStrLn $ "chunkSizes : " ++  show chunkSizes

  	computePids <- forM (zip3 allComputePids chunkOffsets chunkSizes)  $ \(computeNid,cOffset,cSize) -> do
  		say $ printf "[Master] : Spawning CH process on Compute Node :%s" (show computeNid)
  		spawn computeNid ( $(mkClosure 'spawnCHOnCompute) (controllerPid,filePath, cOffset, cSize)) 

  	forM_ computePids $ \computePid -> do
  		say $ printf "[Master-Compute]: Pinging Compute Node %s" (show computePid)
		send computePid (Ping local_pid)

   		(Ping computePid) <- expect
  		say $ printf "[Master-Compute] : Received Ack for further computation from Compute :%s"  (show computePid)

	
  	--forM_ computePids $ \computePid -> do
  	--	say $ printf "[Sending Compute Controller Pid]: Pinging Compute Node for collecting Results %s" (show computePid)
	--	send computePid (Ping controllerPid)

   	--liftIO $ threadDelay 1000


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

  where
  	rtable :: RemoteTable
  	rtable = __remoteTable initRemoteTable
