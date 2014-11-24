{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
import System.Environment
import System.Posix.Process
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)

import Control.Monad
import Text.Printf
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

data PingMessage = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic)          -- <1>

instance Binary PingMessage                 -- <2>

pingServer :: Process ()
pingServer = do
  Ping from <- expect                              -- <1>
  say $ printf "ping received from %s" (show from) -- <2>
  mypid <- getSelfPid                              -- <3>
  send from (Pong mypid)                           -- <4>

remotable ['pingServer]

master :: [NodeId] -> Process ()                     -- <1>
master peers = do

  ps <- forM peers $ \nid -> do                      -- <2>
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid

  forM_ ps $ \pid -> do                              -- <3>
    say $ printf "pinging %s" (show pid)
    send pid (Ping mypid)

  waitForPongs ps                                    -- <4>

  say "All pongs successfully received"
  terminate

waitForPongs :: [ProcessId] -> Process ()            -- <5>
waitForPongs [] = return ()
waitForPongs ps = do
  m <- expect
  case m of
    Pong p -> waitForPongs (filter (/= p) ps)
    _  -> say "MASTER received ping" >> terminate

forkSlavesAndRun :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable)
    -> IO()
forkSlavesAndRun master frtable = do
    args <- getArgs
    path <- getExecutablePath
    let rtable = frtable initRemoteTable
    case args of 
        ["nprocs", nprocs ] -> do
            let n = read nprocs :: Int
            let p = read basePort :: Int
            pids <- mapM (\i -> forkProcess $ executeFile path False 
                ["port", show (p+i), "rank", show i] Nothing) [1..n-1]
            print $ show pids
            print "Done forking"
            liftIO $ threadDelay (1*1000000)
            backend <- initializeBackend defaultHost basePort rtable
            startMaster backend master
        [ "port", port, "rank", rank ] -> do
            print $ "Start slave on port : " ++ port
            backend <- initializeBackend defaultHost port rtable
            startSlave backend
    print "Done"

basePort = "44000"
defaultHost = "localhost"

main :: IO ()
main = forkSlavesAndRun master Main.__remoteTable
