-- | Run.hs
--
-- Functions for starting DNA programs
module DNA.Run (
    dnaRun
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Distributed.Process      (RemoteTable)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Concurrent (threadDelay)
import System.Environment (getExecutablePath)
import System.Process

import DNA.SlurmBackend
import DNA.CmdOpts
import DNA.DNA
import DNA.Monitor


-- | Parse command line option and start program
dnaRun :: (RemoteTable -> RemoteTable) -> DNA () -> IO ()
dnaRun remoteTable dna = do
    opts <- dnaParseOptions
    -- In case of UNIX startup
    case dnaNProcs opts of
      -- SLURM case
      Nothing -> error "SLURM startup is not implemented"
      -- Unix startup
      Just nProc -> runUnix rtable opts nProc dna

  where
    rtable = (remoteTable . __remoteTable) initRemoteTable


runUnix :: RemoteTable -> Options -> Int -> DNA () -> IO ()
runUnix rtable opts nProc dna = do
    let basePort = dnaBasePort opts
        rank     = dnaRank     opts
        port     = basePort + rank
    -- Master process start all other processes
    pids <- case rank of
        0 -> do prog <- getExecutablePath
                forM [1 .. nProc - 1] $ \rnk -> do
                    spawnProcess prog [ "--base-port",     show basePort
                                      , "--internal-rank", show rnk
                                      , "--nprocs",        show nProc
                                      ]
        _ -> return []
    let killChild pid = do
            st <- getProcessExitCode pid
            case st of
              Just _  -> return ()
              Nothing -> terminateProcess pid
    -- FIXME: this is source of potential problems if we get exception
    --        during checking children state
    let reapChildren = mapM_ killChild pids
    -- Initialize backend
    let ports = map (+ basePort) [0 .. nProc - 1]
    backend <- initializeBackend (Local ports) "localhost" (show port) rtable
    -- Start master or slave program
    case rank of
      0 -> do finally (startMaster backend $ \nodes -> do
                            mon <- runMonitor nodes
                            runDNA mon dna
                      ) reapChildren
      _ -> startSlave backend
