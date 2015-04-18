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
import Network.BSD        (getHostName)
import System.Environment (getExecutablePath,getEnv,lookupEnv)
import System.Directory
import System.Process
import System.FilePath    ((</>))
import System.Exit
import System.Posix.Process (getProcessID,executeFile)
import Text.ParserCombinators.ReadP hiding (many)
import Text.Printf

import DNA.SlurmBackend (initializeBackend,startMaster,startSlave,terminateAllSlaves)
import qualified DNA.SlurmBackend as CH
import DNA.CmdOpts
import DNA.Logging
import DNA.Types
import DNA.DSL
import DNA.Interpreter
import DNA.Interpreter.Run
import DNA.Interpreter.Types



-- | Parse command line option and start program
dnaRun :: (RemoteTable -> RemoteTable) -> DNA () -> IO ()
dnaRun remoteTable dna = do
    (opts,common) <- dnaParseOptions
    initLogging (dnaLogger common)
    case opts of
      Unix n        -> runUnix n common
      UnixWorker o  -> runUnixWorker rtable o common dna
      Slurm         -> runSlurm common
      SlurmWorker d -> runSlurmWorker rtable d common dna
  where
    rtable = ( remoteTable
             . DNA.Interpreter.__remoteTable
             . DNA.Interpreter.Run.__remoteTable
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
        [ "+RTS", "-l-au", "-T", "-RTS"
        , "--base-port", show (dnaBasePort common)
        , "--workdir", dir
        ] Nothing

runSlurmWorker :: RemoteTable -> FilePath -> CommonOpt -> DNA () -> IO ()
runSlurmWorker rtable dir common dna = do
    setCurrentDirectory dir
    -- Obtain data from SLURM
    rank     <- slurmRank
    localID  <- slurmLocalID
    hostList <- slurmHosts
    host     <- getHostName
    let port = dnaBasePort common + localID
    -- FIXME: treat several tasks per node correctly
    backend <- initializeBackend
                 (CH.SLURM (dnaBasePort common) hostList)
                 host (show port) rtable
    case rank of
      0 -> startMaster backend $ \n -> do
               synchronizationPoint "CH"
               r <- executeDNA dna n
               terminateAllSlaves backend
               case r of
                 Just e  -> do taggedMessage "FATAL" $ "main actor died: " ++ show e
                               -- FIXME: a bit klunky
                               liftIO exitFailure
                 Nothing -> return ()
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


-- | Obtain list of hosts from SLURM together with number of tasks per
--   host
slurmHosts :: IO [(String,Int)]
slurmHosts = do
    nodeListStr <- getEnv "SLURM_NODELIST"
    numNodesStr <- getEnv "SLURM_TASKS_PER_NODE"
    eventMessage $ "SLURM_NODELIST=" ++ nodeListStr
    eventMessage $ "SLURM_TASKS_PER_NODE=" ++ numNodesStr
    let nodeList = runReadP parseNodeList nodeListStr
        numNodes = runReadP parseNumNode  =<< split ','numNodesStr
    when (length nodeList /= length numNodes) $
        error "Length of SLURM_NODELIST and SLURM_TASKS_PER_NODE does not match"
    return $ nodeList `zip` numNodes

-- Parser for SLURM_TASKS_PER_NODE envvar
parseNumNode :: ReadP [Int]
parseNumNode =  (pure <$> readS_to_P reads <* eof)
     <|> do n <- readS_to_P reads
            _ <- char '('
            _ <-  char 'x'
            r <- readS_to_P reads
            _ <- char ')'
            eof
            return $ replicate r n

-- Parser for SLURM_NODELIST
parseNodeList :: ReadP [String]
parseNodeList = do
    chunks <- list <* eof
    let toStr (Str s)           = [s]
        toStr (Range len (n,m)) = [printf (printf "%%0%ii" len) i | i <- [n .. m]]
        toStr (List rs)         = toStr =<< rs
        toStr (Seq  xs)         = foldl (liftA2 (++)) [""] $ map toStr xs
    return $ toStr =<< chunks
    -- return chunks
  where
    -- Comma-separated list
    list = do r <- many ((Str <$> str) <|> range) `sepBy1` char ','
              return $ map Seq r
    -- Range expression
    range = do _  <- char '['
               rs <- sepBy1 (rangeInt <|> singleInt) (char ',')
               _  <- char ']'
               return $ List rs
    -- Integer ranges
    leadingInt :: ReadP (Int,Int)
    leadingInt = do
        sn <- munch1 $ \c -> c >= '0' && c <= '9'
        n  <- case safeRead sn of
                Just n  -> return n
                Nothing -> empty
        let len = case sn of
                    '0':_ -> length sn
                    _     -> 0
        return (len,n)
    singleInt :: ReadP Chunk
    singleInt = do
        (len,n) <- leadingInt
        return $ Range len (n,n)
    rangeInt :: ReadP Chunk
    rangeInt = do
        (len,n) <- leadingInt
        _ <- char '-'
        m <- int
        return $ Range len (n,m)
    -- String
    str = munch1 $ \c -> or [ c >= 'a' && c <= 'z'
                            , c >= 'A' && c <= 'Z'
                            , c >= '0' && c <= '9'
                            , c == '-'
                            ]
    -- Integer
    int = do s <- munch1 $ \c -> c >= '0' && c <= '9'
             return (read s :: Int)

-- Chunk of NODELIST entry
data Chunk = Str String          -- String chunk
           | Range Int (Int,Int) -- Range chunk: width, (range)
           | List [Chunk]
           | Seq  [Chunk]
           deriving (Show)

runReadP :: ReadP a -> String -> a
runReadP p s = case readP_to_S p s of
    [(a,"")] -> a
    _        -> error $ "Cannot parse string: '" ++ s ++ "'"



----------------------------------------------------------------
-- UNIX start
----------------------------------------------------------------

-- Startup of program
runUnix :: Int -> CommonOpt -> IO ()
runUnix n common = do
    -- Check if SLURM envvars are set and fail they're
    do v <- lookupEnv "SLURM_JOBID"
       case v of
         Nothing -> return ()
         _       -> error "SLURM environment variable is set will not use UNIX startup"
    --
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
        [ "+RTS", "-l-au", "-T", "-RTS"
        , "--nprocs",        show n
        , "--internal-rank", "0"
        , "--internal-pid",  dnaPID
        , "--workdir",       workdir
        , "--base-port",     show (dnaBasePort common)
        , "-v",              show $ logOptVerbose (dnaLogger common)
        , "--measure",       logOptMeasure (dnaLogger common)
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
                        [ "+RTS", "-l-au", "-T", "-RTS"
                        , "--base-port",     show basePort
                        , "--nprocs",        show nProc
                        , "--internal-rank", show rnk
                        , "--internal-pid",  dnaPID
                        , "--workdir",       dnaUnixWDir opts
                        , "-v",              show $ logOptVerbose (dnaLogger common)
                        , "--measure",       logOptMeasure (dnaLogger common)
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
            r <- executeDNA dna n
            terminateAllSlaves backend
            case r of
              Just e  -> do taggedMessage "FATAL" $ "main actor died: " ++ show e
                            liftIO $ print e
                            liftIO exitFailure
              Nothing -> return ()
    -- Start master or slave program
    case rank of
      0 -> start `onException` reapChildren
      _ -> startSlave backend


executeDNA :: DNA () -> [NodeId] -> Process (Maybe SomeException)
executeDNA dna nodes = do
    me  <- getSelfPid
    nid <- getSelfNode
    let param = ActorParam
              { actorParent      = me -- FIXME???
              , actorInterpreter = $(mkStaticClosure 'theInterpreter)
              , actorRank        = Rank 0
              , actorGroupSize   = GroupSize 1
              , actorNodes       = VirtualCAD Local (NodeInfo nid) (map NodeInfo nodes)
              , actorDebugFlags  = []
              }
    r <- try $ runDnaParam param dna
    return $ either Just (const Nothing) r


safeRead :: Read a => String -> Maybe a
safeRead s = do
    [(a,"")] <- Just $ reads s
    return a

split :: Char -> String -> [String]
split c str =
    case break (==c) str of
      (a,"")  -> [a]
      (a,_:s) -> a : split c s
