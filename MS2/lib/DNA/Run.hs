{-# LANGUAGE TemplateHaskell #-}
-- | Run.hs
--
-- Functions for starting DNA programs
module DNA.Run (
    dnaRun
  ) where

import Control.Monad
import Control.Exception
import Control.Distributed.Process      hiding (finally)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import System.Environment (getExecutablePath,getEnv)
import System.Process
import System.FilePath    ((</>))
import qualified Data.Foldable as T

import DNA.SlurmBackend (initializeBackend,startMaster,startSlaveWithProc)
import qualified DNA.SlurmBackend as CH
import DNA.CmdOpts
import DNA.DNA        hiding (__remoteTable,rank)
import DNA.Controller hiding (__remoteTable)
import DNA.Types
import qualified DNA.DNA
import qualified DNA.Controller



-- | Parse command line option and start program
dnaRun :: (RemoteTable -> RemoteTable) -> DNA () -> IO ()
dnaRun remoteTable dna = do
    opts <- dnaParseOptions
    -- Create directory for logs
    home <- getEnv "HOME"
    let logDir = home </> "_dna" </> "logs" </> dnaPID opts </> show (dnaRank opts)
    -- In case of UNIX startup
    case dnaNProcs opts of
      -- SLURM case
      Nothing -> error "SLURM startup is not implemented"
      -- Unix startup
      Just nProc -> runUnix logDir rtable opts nProc dna
  where
    rtable = ( remoteTable
             . DNA.DNA.__remoteTable
             . DNA.Controller.__remoteTable
             ) initRemoteTable


runUnix :: FilePath -> RemoteTable -> Options -> Int -> DNA () -> IO ()
runUnix logDir rtable opts nProc dna = do
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
    backend <- initializeBackend (CH.Local ports) "localhost" (show port) rtable
    -- Start master or slave program
    case rank of
      0 -> startMaster backend (executeDNA logDir dna) `finally` reapChildren
      _ -> startSlaveWithProc backend (startLoggerProcess logDir)


executeDNA :: FilePath -> DNA () -> [NodeId] -> Process ()
executeDNA logDir dna nodes = do
    me  <- getSelfPid
    nid <- getSelfNode
    _   <- spawnLocal $ startLoggerProcess logDir
    -- Create CAD out of list of nodes
    let initialCAD = makeCAD (nid : nodes)
    -- FIXME: very-very-very bad
    CAD n rest <- spawnHierachically initialCAD
    let param = ParamACP
            { acpSelf  = acpClos
            , acpActorClosure = ()
            , acpVCAD = VirtualCAD Local n (T.toList =<< rest)
            , acpActor = ParamActor
                { actorParentACP = me
                , actorRank      = Rank 0
                , actorGroupSize = GroupSize 1
                }
            }
        acpClos = $(mkStaticClosure 'runACP)
    -- Start master ACP
    runMasterACP param dna
