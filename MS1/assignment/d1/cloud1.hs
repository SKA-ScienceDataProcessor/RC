{-# LANGUAGE TemplateHaskell		#-}
{-# LANGUAGE DeriveDataTypeable		#-}
{-# LANGUAGE DeriveGeneric		#-}
{-# LANGUAGE GeneralizedNewtypeDeriving	#-}

module Main where

import Text.Printf
import Data.Binary
import Data.Typeable
import Control.Monad
import System.Posix.Files
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)

newtype Message = Ping ProcessId deriving (Eq, Ord, Binary, Typeable)

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

itemSize = 8

filePath :: FilePath
filePath="float_file.txt"

spawnCHOnController :: Process ()
spawnCHOnController = do

     	local_pid <- getSelfPid
	say $ printf "[Controller] : Waiting for master instruction!! : %s" (show local_pid)	

     	(Ping remote_pid) <- expect
     	say $ printf "[Controller] : Ping from master :%s" (show remote_pid)

     	send remote_pid (Ping local_pid)
     	say $ printf "[Controller] : Ready for further computation :%s" (show local_pid)

spawnCHOnSlave :: (FilePath, [int], [int]) -> Process ()
spawnCHOnSlave (file, chunkOffsets, chunkSizes) = do

       	local_pid <- getSelfPid
	say $ printf "[Slave] : Waiting for master instruction!! :%s" (show local_pid)	
	forever $ do
		(Ping remote_pid) <- expect
       		say $ printf "[Slave] : Ping from master :%s on slave: %s" (show remote_pid) (show local_pid) 

		send remote_pid (Ping local_pid)
		say $ printf "[Slave] : Ready for further computation :%s" (show local_pid)


remotable [ 'spawnCHOnSlave, 'spawnCHOnController ]

master :: Backend -> [NodeId] -> Process ()
master backend peers = do

	local_pid <- getSelfPid
  	say $ printf "[Master] : Master Initiated !! :%s" (show local_pid)

	-- Reading first NodeID for controller
  	let controllerNid = head peers
  	say $ printf "[Master] : Spawning CH process on Controller Node :%s" (show controllerNid)
  	controllerPid <- spawn controllerNid $(mkStaticClosure 'spawnCHOnController)

  	say $ printf "[Master-Controller] : Pinging Controller Node %s" (show controllerPid)
  	send controllerPid (Ping local_pid)

  	(Ping controllerPid) <- expect
  	say $ printf "[Master-Controller] : Received Ack for further computation from Controller :%s"  (show controllerPid)
	
	say "--------------------------------------------------------------------------------------"	

  	let allSlavePids = tail peers
  	--slavePids <- forM allSlavePids $ \slaveNid -> do
      	--	say $ printf "[Master] : Spawning CH process on Slave Node :%s" (show slaveNid)
      	--	spawn slaveNid $ $(mkStaticClosure 'spawnCHOnSlave) ()


  	--forM_ slavePids $ \slavePid -> do
    	--	say $ printf "[Master-Slave]: Pinging Slave Node %s" (show slavePid)
    	--	send slavePid (Ping local_pid)

    	--	(Ping slavePid) <- expect
    	--	say $ printf "[Master-Slave] : Received Ack for further computation from Slave :%s"  (show slavePid)
	
	let chunkCount = length peers
	fileStatus <- liftIO $ getFileStatus filePath 
	let itemCount = div (read $ show (fileSize fileStatus)) itemSize
	liftIO . putStrLn $ "itemcount:  " ++  (show itemCount)
	
	let chunkOffsets = map (chunkOffset chunkCount itemCount) [1..chunkCount]
	liftIO . putStrLn $ "Offsets : " ++ show chunkOffsets
     	let chunkSizes = map (chunkSize chunkCount itemCount) [1..chunkCount]
     	liftIO . putStrLn $ "chunkSizes : " ++  show chunkSizes


        slavePids <- forM_ allSlavePids $ \slaveNid -> do
                say $ printf "[Master] : Spawning CH process on Slave Node :%s" (show slaveNid)
     		spawn slaveNid ( $(mkClosure 'spawnCHOnSlave) (filePath, chunkOffsets, chunkSizes )) 

  	forM_ slavePids $ \slavePid -> do
    		say $ printf "[Master-Slave]: Pinging Slave Node %s" (show slavePid)
		send slavePid (Ping local_pid)

  	  	(Ping slavePid) <- expect
    		say $ printf "[Master-Slave] : Received Ack for further computation from Slave :%s"  (show slavePid)
	
   	--liftIO $ threadDelay 1000


  	terminateAllSlaves backend	

main :: IO ()
main = do
        args <- getArgs
	--let config = head allCmdLine
	--let cadList = tail allCmdLine
	--print $ show config
	--print $ show cadList

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
