-- | Run.hs
--
-- Functions for starting DNA programs
module DNA.Run (
    dnaRun
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)

import DNA.SimpleLocalNetWithoutDiscovery
import DNA.CmdOpts
import DNA.DNA
import DNA.Monitor

-- | Parse command line option and start program
dnaRun :: (RemoteTable -> RemoteTable) -> DNA () -> IO ()
dnaRun remoteTable dna = do
    options <- dnaParseCommandLineOpts
    let rtable = (remoteTable . __remoteTable) initRemoteTable
    case options of
        Master (CommonOpts cadFile ip port) masterOptions -> do
            backend <- initializeBackend cadFile ip port rtable
            startMaster backend $ \nodes -> do
                mon <- runMonitor nodes
                runDNA mon dna
        Slave (CommonOpts cadFile ip port) -> do
            backend <- initializeBackend cadFile ip port rtable
            startSlave backend
