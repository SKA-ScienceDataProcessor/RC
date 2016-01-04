{-# OPTIONS_HADDOCK hide #-}
-- |
--
-- Command-line options parsing, using optparser-applicative.
--
--
--
-- Copyright (C) 2014 Braa Research, LLC.
module DNA.CmdOpts (
      StartOpt(..)
    , UnixStart(..)
    , CommonOpt(..)
    , dnaParseOptions
    ) where

import Control.Monad
import Options.Applicative
import System.Environment

import DNA.Logging


-- | Options for different methods of starting DNA program
data StartOpt
    = Unix       Int            -- ^ UNIX start
    | UnixWorker UnixStart
    | Slurm
    | SlurmWorker FilePath
    deriving (Show)

-- | Options for UNIX start
data UnixStart = UnixStart
    { dnaUnixNProc :: Int       -- ^ Total N of processes
    , dnaUnixPID   :: String    -- ^ PID of process
    , dnaUnixRank  :: Int       -- ^ Rank of process
    , dnaUnixWDir  :: FilePath  -- ^ Working directory for program
    }
    deriving (Show)


-- | Options common for all method of starting DNA program.
data CommonOpt = CommonOpt
    { dnaBasePort :: Int        -- ^ Base port for program
    , dnaLogger   :: LoggerOpt  -- ^ Logger options
    }
    deriving (Show)


-- | Obtain startup options for
dnaParseOptions :: IO (StartOpt, CommonOpt)
dnaParseOptions = do
    -- Parse command line parameters
    args <- getArgs
    -- FIXME: Alternative instance doesn't work properly for composite
    --        parsers. Probably bug should be filed for that
    let a <.> b = (,) <$> a <*> b
    let run p = execParserPure ParserPrefs
                    { prefMultiSuffix     = "VAR"
                    , prefDisambiguate    = False
                    , prefShowHelpOnError = True
                    , prefBacktrack       = True
                    , prefColumns         = 80
                    }
                    (wrapParser p) args
    -- FIXME: better error reporting required
    case run (optUnixWorker <.> optCommon) of
      Success a -> return a
      _ -> case run (optUnix <.> optCommon) of
             Success a -> return a
             _ -> case run (optSlurmWorker <.> optCommon) of
                   Success a -> return a
                   _ -> case run (optSlurm <.> optCommon) of
                       Success a -> return a
                       _ -> error "Cannot parse command line parameters"
  where
    wrapParser p = info (helper <*> p)
        (  fullDesc
        <> progDesc "Start DNA program"
        <> header "DNA Cloud Haskell implementation"
        )
    --
    optUnix       = Unix <$> optNProcs
    optUnixWorker = UnixWorker <$>
        ( UnixStart
       <$> optNProcs
       <*> optPID
       <*> optRank
       <*> optWDir
        )
    optSlurm  = pure Slurm
    optSlurmWorker = SlurmWorker <$> optWDir
    optCommon = CommonOpt <$> optBasePort  <*> optLogger
    optLogger = LoggerOpt <$> optVerbosity <*> pure NoDebugPrint <*> optMeasure
    -- Parsers for
    optWDir = option str
            ( metavar "DIR"
           <> long "workdir"
           <> help "Working directory for program"
            )
    optRank = option positiveNum
            ( metavar "RANK"
           <> long "internal-rank"
           <> hidden
           <> help "specify rank of process [DO NOT USE MANUALLY!]"
            )
           <|> pure 0
    optPID = option str
           ( metavar "PID"
          <> long    "internal-pid"
          <> hidden
          <> help    "specify Job ID of process"
           )
    optBasePort = option readPort
                ( metavar "PORT"
               <> long "base-port"
               <> help "set base port for processes (default 40000)"
                )
               <|> pure 40000
    optNProcs = option positiveNum
              ( metavar "N"
             <> long "nprocs"
             <> help "number of processe to spawn"
              )
    optVerbosity = option positiveNum
              ( metavar "VERBOSE"
              <> short 'v'
              <> help "steers amount of log messages to output"
              )
              <|> pure 0
    optMeasure = option str
               ( metavar "MEASURE"
               <> long "measure"
               <> help "do accurate measurements of metrics, at the expense of performance"
               )
               <|> pure ""

positiveNum :: (Read a, Ord a, Num a) => ReadM a
positiveNum = do
    a <- auto
    guard (a >= 0)
    return a

readPort :: ReadM Int
readPort = do
    a <- auto
    guard (a > 0 && a < 65535)
    return a
