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
import System.Directory
import System.Process
import System.FilePath    ((</>))
import System.Posix.Process (getProcessID,executeFile)
import qualified Data.Foldable as T

import DNA.SlurmBackend (initializeBackend,startMaster,startSlaveWithProc)
import qualified DNA.SlurmBackend as CH
import DNA.CmdOpts
import DNA.DNA        hiding (__remoteTable,rank)
import DNA.Controller hiding (__remoteTable)
import DNA.Logging
import DNA.Types
import qualified DNA.DNA
import qualified DNA.Controller



-- | Parse command line option and start program
dnaRun :: (RemoteTable -> RemoteTable) -> DNA () -> IO ()
dnaRun remoteTable dna = do
    (opts,common) <- dnaParseOptions
    case opts of
      Unix n       -> runUnix n common
      UnixWorker o -> runUnixWorker rtable o common dna
      Slurm        -> error "SLURM startup is not implemented"
  where
    rtable = ( remoteTable
             . DNA.DNA.__remoteTable
             . DNA.Controller.__remoteTable
             ) initRemoteTable

-- Startup of program
runUnix :: Int -> CommonOpt -> IO ()
runUnix n common = do
    pid     <- getProcessID
    program <- getExecutablePath
    workdir <- getCurrentDirectory
    -- Create log dirs for all processess
    home    <- getEnv "HOME"
    let dnaPID = show pid ++ "-u"
        logDir = home </> "_dna" </> "logs" </> dnaPID
    forM_ [0 .. n-1] $ \k ->
        createDirectoryIfMissing True (logDir </> show k)
    -- Reexec program with enabled eventlog
    setCurrentDirectory (logDir </> "0")
    executeFile program True
        [ "+RTS", "-l-au", "-RTS"
        , "--nprocs",        show n
        , "--internal-rank", "0"
        , "--internal-pid",  dnaPID
        , "--workdir",       workdir
        , "--base-port",     show (dnaBasePort common)
        ] Nothing

runUnixWorker :: RemoteTable -> UnixStart -> CommonOpt -> DNA () -> IO ()
runUnixWorker rtable opts common dna = do
    home    <- getEnv "HOME"
    let basePort = dnaBasePort common
        rank     = dnaUnixRank opts
        port     = basePort + rank
        dnaPID   = dnaUnixPID opts
        nProc    = dnaUnixNProc opts
    let logDir = home </> "_dna" </> "logs" </> dnaPID
    -- Spawn child processes
    pids <- case rank of
        0 -> do program <- getExecutablePath
                forM [1 .. nProc - 1] $ \rnk -> do
                    let dir = logDir </> show rnk
                    createDirectoryIfMissing True dir
                    setCurrentDirectory dir
                    spawnProcess program
                        [ "+RTS", "-l-au", "-RTS"
                        , "--base-port",     show basePort
                        , "--nprocs",        show nProc
                        , "--internal-rank", show rnk
                        , "--internal-pid",  dnaPID
                        , "--workdir",       dnaUnixWDir opts
                        ]
        _ -> return []
    -- Go back to working dir
    setCurrentDirectory (dnaUnixWDir opts)
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
