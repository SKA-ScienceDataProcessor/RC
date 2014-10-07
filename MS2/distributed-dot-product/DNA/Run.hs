-- | Run.hs
--
-- Functions for starting DNA programs
module DNA.Run where

import Control.Concurrent
import Control.Distributed.Process

import DNA.SimpleLocalNetWithoutDiscovery
import DNA.CmdOpts


-- | Parse command line option and start program
dnaParseCommandLineAndRun :: RemoteTable -> (MasterOptions -> Backend -> [NodeId] -> Process ()) -> IO ()
dnaParseCommandLineAndRun remoteTable master = do
    options <- dnaParseCommandLineOpts
    case options of
        Master (CommonOpts cadFile ip port) masterOptions -> do
            backend <- initializeBackend cadFile ip port remoteTable
            startMaster backend (master masterOptions backend)
            liftIO $ threadDelay 100
        Slave (CommonOpts cadFile ip port) -> do
            backend <- initializeBackend cadFile ip port remoteTable
            startSlave backend
