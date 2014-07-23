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
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS

newtype Message = Ping ProcessId deriving (Eq, Ord, Show, Binary, Typeable)

newtype PartialSum = PartialSum Double deriving (Show,Typeable,Binary)

roundUpDiv:: Int -> Int -> Int
roundUpDiv a b = - div (-a) b

chunkOffset :: Int -> Int -> Int -> Int
chunkOffset chunkCount itemCount chunkNo
	| chunkNo > chunkCount || chunkNo < 1 = -1
  | otherwise = (chunkNo -1 ) * roundUpDiv itemCount chunkCount


chunkSize :: Int -> Int -> Int -> Int
chunkSize cC iC cN
	|  cN < 1 || cN > cC = 0
  |  cN > div iC (roundUpDiv iC cC) = iC - (cN -1) * (roundUpDiv iC cC)
  |  otherwise = roundUpDiv iC cC

foreign import ccall unsafe "buffer-io.h read_data"
  c_read_data :: Ptr CDouble -> CInt -> IO ()

-- Then we can read it into storable vector. Or any similar data
-- -- structure which uses buffers residing in pinned memory
-- -- (e.g. bytestring). Pinned memory is not moved by GC so it's safe to
-- -- pass pointers to C.
readData :: Int -> IO (S.Vector Double)
readData n = do
	mv <- MS.new n :: IO (MS.IOVector Double)
  	MS.unsafeWith mv $ \ptr ->
    -- Here I assume that CDouble and Double are same thing (it is)
    --     -- and blindly cast pointer
        	c_read_data (castPtr ptr) (fromIntegral n)
        S.unsafeFreeze mv



itemSize = 8

filePath :: FilePath
filePath="float_file.txt"

spawnCHOnCollector :: [NodeId] -> Process ()
spawnCHOnCollector slavePids = do


  	local_pid <- getSelfPid
	say $ printf "[Collector] : Waiting for master instruction!! : %s" (show local_pid)	

  	(Ping remote_pid) <- expect
  	say $ printf "[Collector] : Ping from master :%s" (show remote_pid)

  	send remote_pid (Ping local_pid)
  	say $ printf "[Collector] : Ready for further computation :%s" (show local_pid)

	forM_ slavePids $ \slavePid -> do
                say $ printf "[Collector-Slave]: Expecting RESULT from Slave Node %s" (show slavePid)
                (PartialSum localSum) <- expect
                say $ printf "[Collector]: RESULT : %s from Slave Node : %s " (show localSum) (show slavePid)

spawnCHOnSlave :: (FilePath, Int, Int, Int) -> Process ()
spawnCHOnSlave (file, chunkOffset, chunkSize, itemCount) = do

	local_pid <- getSelfPid
	say $ printf "[Slave] : Waiting for master instruction!! :%s" (show local_pid)	
	say $ printf "[Slave] : itemCount :%s " (show itemCount)
	forever $ do
		(Ping remote_pid) <- expect
    		say $ printf "[Slave] : Ping from PID :%s on slave: %s" (show remote_pid) (show local_pid) 

		send remote_pid (Ping local_pid)
		say $ printf "[Slave] : Ready for further computation :%s" (show local_pid)

		say $ printf "[Slave] : Value of chunkOffset : [[%s]] at %s" (show chunkOffset) (show local_pid)
		say $ printf "[Slave] : Value of chunkSize : [[%s]] at %s" (show chunkSize) (show local_pid)
	

		iov <- liftIO $ readData itemCount
		say $ printf "[Slave] : Value of iov: %s at %s" (show iov) (show local_pid)		
		
		let sumOnComputeNode = S.sum iov
		say $ printf "[Slave] : sumOnComputeNode : %s at %s" (show sumOnComputeNode) (show local_pid)

		say $ printf "[Slave] : Sending results to Controller : %s" (show remote_pid)
		send remote_pid (PartialSum sumOnComputeNode)


remotable [ 'spawnCHOnSlave, 'spawnCHOnCollector ]

master :: Backend -> [NodeId] -> Process ()
master backend peers = do

	local_pid <- getSelfPid
 	say $ printf "[Master] : Master Initiated !! :%s" (show local_pid)

  	let allSlavePids = tail peers
	let chunkCount = length allSlavePids 
	fileStatus <- liftIO $ getFileStatus filePath 
	let itemCount = div (read $ show (fileSize fileStatus)) itemSize
	liftIO . putStrLn $ "itemcount:  " ++  (show itemCount)
	
	let chunkOffsets = map (chunkOffset chunkCount itemCount) [1..chunkCount]
	liftIO . putStrLn $ "Offsets : " ++ show chunkOffsets
  	let chunkSizes = map (chunkSize chunkCount itemCount) [1..chunkCount]
  	liftIO . putStrLn $ "chunkSizes : " ++  show chunkSizes


  	slavePids <- forM (zip3 allSlavePids chunkOffsets chunkSizes)  $ \(slaveNid,cOffset,cSize) -> do
  		say $ printf "[Master] : Spawning CH process on Slave Node :%s" (show slaveNid)
  		spawn slaveNid ( $(mkClosure 'spawnCHOnSlave) (filePath, cOffset, cSize, itemCount)) 
  	

  	forM_ slavePids $ \slavePid -> do
  		say $ printf "[Master-Slave]: Pinging Slave Node %s" (show slavePid)
		send slavePid (Ping local_pid)

   		(Ping slavePid) <- expect
  		say $ printf "[Master-Slave] : Received Ack for further computation from Slave :%s"  (show slavePid)


	say "--------------------------------------------------------------------------------------"	

	-- Reading first NodeID for controller
 	let controllerNid = head peers
 	say $ printf "[Master] : Spawning CH process on Collector Node :%s" (show controllerNid)
 	controllerPid <- spawn controllerNid ( $(mkClosure 'spawnCHOnCollector) (allSlavePids))

  	forM_ slavePids $ \slavePid -> do
  		say $ printf "[Sending Slave Controller Pid]: Pinging Slave Node for collecting Results %s" (show slavePid)
		send slavePid (Ping controllerPid)

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
