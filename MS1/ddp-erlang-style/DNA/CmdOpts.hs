-- |CmdLine.hs
--
-- Command-line options parsing, using optparser-applicative.
--
-- Copyright (C) 2014 Braa Research, LLC.

module DNA.CmdOpts (
          Options(..)
        , dnaParseCommandLineOpts
        , dnaParseCommandLineAndRun
        ) where

import Control.Concurrent
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet

import Options.Applicative

-- |@Options@ data type. Holds information parsed from command line.
-- It is a record, main field is optionsRunMode 
data Options =
                Master  Bool    String  String
        |       Slave   String  String
        deriving (Show)

-- |Parse options.
optionsParser :: String -> ParserInfo Options
optionsParser name = info (helper <*> (subparser (master <> slave)))
                (  fullDesc
                <> progDesc "run a distributed dot product program as master or slave on IP:PORT binding"
                <> header (name++" Cloud Haskell implementation")
                )
        where
                master = command "master"
                        (info (Master <$> crashOpt <*> ip <*> port) (progDesc "run as master on IP:PORT"))
                slave = command "slave"
                        (info (Slave <$> ip <*> port) (progDesc "run as slave on IP:PORT"))
                crashOpt :: Parser Bool
                crashOpt = switch (long "crash" <> short 'c' <> help "make one node to crash.")
                ip = strOption (metavar "IP" <> long "ip" <> short 'i' <> help "IP address")
                port = strOption (metavar "PORT" <> long "port" <> short 'p' <> help "port for binnding")

-- |An interface to parse command line options.
-- Returns options parsed as data type @Options@.
dnaParseCommandLineOpts :: String -> IO Options
dnaParseCommandLineOpts progName = do
        execParser $ optionsParser progName


dnaParseCommandLineAndRun :: RemoteTable -> String -> (Backend -> [NodeId] -> Process ()) -> IO ()
dnaParseCommandLineAndRun remoteTable progName master= do
        options <- dnaParseCommandLineOpts progName
        case options of
                Master crash ip port -> do
                        backend <- initializeBackend ip port remoteTable
                        startMaster backend (master backend)
                        liftIO $ threadDelay 100
                Slave ip port -> do
                        backend <- initializeBackend ip port remoteTable
                        startSlave backend
