-- |CmdLine.hs
--
-- Command-line options parsing, using optparser-applicative.
--
--
--
-- Copyright (C) 2014 Braa Research, LLC.
module DNA.CmdOpts (
      Options(..)
    , dnaParseOptions
    ) where

import Control.Monad
import Options.Applicative
import System.Environment   (lookupEnv)
import System.Posix.Process (getProcessID)


-- | Program options. Noe that we obtain configuration from both
--   environment and from command line
data Options = Options
    { dnaPID      :: String     -- ^ Process ID
    , dnaRank     :: Int        -- ^ Rank of UNIX process
    , dnaNProcs   :: Maybe Int  -- ^ Number of processes to
                                --   spawn. Nothing in SLURM case
    , dnaBasePort :: Int        -- ^ Base port for program
    }
    deriving (Show)

-- | Obtain startup options for
dnaParseOptions :: IO Options
dnaParseOptions = do
    -- Try to obtain envvars set by SLURM
    mslurmJID <- lookupEnv "SLURM_JOBID"
    mslurmRnk <- (safeRead =<<) <$> lookupEnv "SLURM_PROCID"
    -- They must be eitherboth present of both absent
    case (mslurmJID,mslurmRnk) of
      (Nothing,Nothing)             -> do
          pid <- getProcessID
          execParser $ wrapParser $  Options
                                 <$> optPID pid
                                 <*> optRank
                                 <*> (Just <$> optNProcs)
                                 <*> optBasePort
      (Just slurmJID,Just slurmRnk) -> do
          execParser $ wrapParser $ Options
                                 <$> pure ("s-" ++ slurmJID)
                                 <*> pure slurmRnk
                                 <*> pure Nothing
                                 <*> optBasePort
      _ -> error "SLURM envvars"
  where
    wrapParser p = info (helper <*> p)
        (  fullDesc
        <> progDesc "Start DNA program"
        <> header ("DNA Cloud Haskell implementation")
        )
    -- Command line options
    optRank = option positiveNum
            ( metavar "RANK"
           <> long "internal-rank"
           <> hidden
           <> help "specify rank of process [DO NOT USE MANUALLY!]"
            )
           <|> pure 0
    optPID p = option str
             ( metavar "PID"
            <> long    "internal-pid"
            <> hidden
            <> help    "specify Job ID of process"
             ) <|> pure ("u-" ++ show p)
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

safeRead :: Read a => String -> Maybe a
safeRead s = do
    [(a,"")] <- Just $ reads s
    return a
