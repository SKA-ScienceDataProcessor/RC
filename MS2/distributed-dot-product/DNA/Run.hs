-- | Run.hs
--
-- Functions for starting DNA programs
module DNA.Run (
    dnaRun
  ) where

import Control.Concurrent
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)

import DNA.SimpleLocalNetWithoutDiscovery
import DNA.CmdOpts


-- | Parse command line option and start program
dnaRun :: (RemoteTable -> RemoteTable)
       -> (MasterOptions -> Backend -> [NodeId] -> Process ())
       -> IO ()
dnaRun remoteTable master = do
    options <- dnaParseCommandLineOpts
    let rtable = remoteTable initRemoteTable
    case options of
        Master (CommonOpts cadFile ip port) masterOptions -> do
            backend <- initializeBackend cadFile ip port rtable
            startMaster backend (master masterOptions backend)
            liftIO $ threadDelay 100
        Slave (CommonOpts cadFile ip port) -> do
            backend <- initializeBackend cadFile ip port rtable
            startSlave backend
