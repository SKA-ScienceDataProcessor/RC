-- | Run.hs
--
-- Functions for starting DNA programs
module DNA.Run (
    dnaRun
  ) where

import Control.Monad
import Control.Exception
import Control.Distributed.Process      hiding (finally)
import Control.Distributed.Process.Node (initRemoteTable)
import System.Environment (getExecutablePath,getEnv)
import System.Process
import System.Directory   (createDirectoryIfMissing)
import System.FilePath    ((</>))

import DNA.SlurmBackend
import DNA.CmdOpts
import DNA.DNA        hiding (__remoteTable,rank)
import DNA.Controller hiding (__remoteTable)
import qualified DNA.DNA
import qualified DNA.Controller


-- | Parse command line option and start program
dnaRun :: (RemoteTable -> RemoteTable) -> DNA () -> IO ()
dnaRun remoteTable dna = do
    opts <- dnaParseOptions
    -- Create directory for logs
    home <- getEnv "HOME"
    createDirectoryIfMissing True $
        home </> "_dna" </> "logs" </> dnaPID opts </> show (dnaRank opts)
    -- In case of UNIX startup
    case dnaNProcs opts of
      -- SLURM case
      Nothing -> error "SLURM startup is not implemented"
      -- Unix startup
      Just nProc -> runUnix rtable opts nProc dna

  where
    rtable = ( remoteTable
             . DNA.DNA.__remoteTable
             . DNA.Controller.__remoteTable
             ) initRemoteTable


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
                                      , "--internal-pid",  dnaPID opts
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
      0 -> startMaster backend (executeDNA dna) `finally` reapChildren
      _ -> startSlave backend


executeDNA :: DNA () -> [NodeId] -> Process ()
executeDNA dna nodes = do
    -- Create CAD out of list of nodes
    let initialCAD = makeCAD nodes
    cad <- spawnHierachically initialCAD
    -- Start master's ACP
    return ()
