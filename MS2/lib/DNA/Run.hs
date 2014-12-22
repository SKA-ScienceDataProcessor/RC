{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Run.hs
--
-- Functions for starting DNA programs
module DNA.Run (
    dnaRun
  ) where

import Control.Applicative
import Control.Monad
import Control.Exception (onException,SomeException)
import Control.Distributed.Process      hiding (onException)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import System.Environment (getExecutablePath,getEnv,lookupEnv)
import System.Directory
import System.Process
import System.FilePath    ((</>))
import System.Posix.Process (getProcessID,executeFile)
import qualified Data.Foldable as T

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

import DNA.SlurmBackend (initializeBackend,startMaster,startSlave,terminateAllSlaves)
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
      Unix n        -> runUnix n common
      UnixWorker o  -> runUnixWorker rtable o common dna
      Slurm         -> runSlurm common
      SlurmWorker d -> runSlurmWorker rtable d common dna
  where
    rtable = ( remoteTable
             . DNA.DNA.__remoteTable
             . DNA.Controller.__remoteTable
             ) initRemoteTable


----------------------------------------------------------------
-- SLURM
----------------------------------------------------------------

runSlurm :: CommonOpt -> IO ()
runSlurm common = do
    dir       <- getCurrentDirectory
    mslurmJID <- lookupEnv "SLURM_JOBID"
    slurmRnk <- slurmRank
    let slurmJID = case mslurmJID of
            Just s  -> s ++ "-s"
            Nothing -> error "SLURM_JOBID is not set!"
    -- Cd to log dir and reexec
    home <- getEnv "HOME"
    let logDir = home </> "_dna" </> "logs" </> slurmJID </> show slurmRnk
    createDirectoryIfMissing True logDir
    setCurrentDirectory logDir
    program <- getExecutablePath
    executeFile program True
        [ "+RTS", "-l-au", "-RTS"
        , "--workdir", dir
        ] Nothing

runSlurmWorker :: RemoteTable -> FilePath -> CommonOpt -> DNA () -> IO ()
runSlurmWorker rtable dir common dna = do
    setCurrentDirectory dir
    --
    rank    <- slurmRank
    localID <- slurmLocalID
    hosts   <- slurmHosts
    let port = dnaBasePort common + localID
        -- FIXME: is localhost OK?
        host = "localhost"
    -- FIXME: treat several tasks per node correctly
    backend <- initializeBackend
                 (CH.SLURM (dnaBasePort common) [(h,1) | h <- hosts])
                 host (show port) rtable
    case rank of
      0 -> startMaster backend $ \n -> do
               synchronizationPoint "CH"
               executeDNA dna n
               terminateAllSlaves backend
      _ -> startSlave backend

slurmRank :: IO Int
slurmRank = do
    mslurmRnk <- (safeRead =<<) <$> lookupEnv "SLURM_PROCID" :: IO (Maybe Int)
    case mslurmRnk of
      Just r  -> return r
      Nothing -> error "SLURM_PROCID is not set!"

slurmLocalID :: IO Int
slurmLocalID = do
    mslurmRnk <- (safeRead =<<) <$> lookupEnv "SLURM_LOCALID" :: IO (Maybe Int)
    case mslurmRnk of
      Just r  -> return r
      Nothing -> error "SLURM_LOCALID is not set!"


-- | Obtain list of hosts from SLURM
slurmHosts :: IO [String]
slurmHosts = do
    nodeStr <- getEnv "SLURM_NODELIST"
    nodes   <- withCString nodeStr $ \s -> c_slurm_hostlist_create s
    let loop = do
            p <- c_slurm_hostlist_shift nodes
            if p == nullPtr
               then return []
               else do ss <- loop
                       s  <- peekCString p
                       return (s : ss)
    s <- loop
    c_slurm_hostlist_destroy nodes
    return s


-- | Tag for hostlist_t. Note hostlist_t is pointer to struct hostlist
data Hostlist

foreign import ccall safe "slurm_hostlist_create"
    c_slurm_hostlist_create :: CString -> IO (Ptr Hostlist)

foreign import ccall safe "slurm_hostlist_shift"
    c_slurm_hostlist_shift :: Ptr Hostlist -> IO CString

foreign import ccall safe "slurm_hostlist_destroy"
    c_slurm_hostlist_destroy :: Ptr Hostlist -> IO ()



----------------------------------------------------------------
-- UNIX start
----------------------------------------------------------------

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
    synchronizationPoint "UNIX"
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
    -- Initialize backend
    let ports = map (+ basePort) [0 .. nProc - 1]
    backend <- initializeBackend (CH.Local ports) "localhost" (show port) rtable
    -- FIXME: this is source of potential problems if we get exception
    --        during checking children state
    let reapChildren = mapM_ killChild pids
        start = startMaster backend $ \n -> do
            synchronizationPoint "CH"
            executeDNA dna n
            terminateAllSlaves backend
    -- Start master or slave program
    case rank of
      0 -> start `onException` reapChildren
      _ -> startSlave backend



executeDNA :: DNA () -> [NodeId] -> Process ()
executeDNA dna nodes = do
    me  <- getSelfPid
    nid <- getSelfNode
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
    _ <- try $ runMasterACP param dna :: Process (Either SomeException ())
    return ()

safeRead :: Read a => String -> Maybe a
safeRead s = do
    [(a,"")] <- Just $ reads s
    return a

