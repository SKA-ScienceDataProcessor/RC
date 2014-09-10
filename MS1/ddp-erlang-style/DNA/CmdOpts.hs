-- |CmdLine.hs
--
-- Command-line options parsing, using optparser-applicative.
--
-- Copyright (C) 2014 Braa Research, LLC.

module DNA.CmdOpts (
          Options(..)
        , MasterOptions(..)
        , dnaParseCommandLineOpts
        , dnaParseCommandLineAndRun
        ) where

import Control.Concurrent
import Control.Distributed.Process
import DNA.SimpleLocalNetWithoutDiscovery

import Options.Applicative

import DNA.Channel.File (requiredAlignmentInItems, Int64)

-- |@Options@ data type. Holds information parsed from command line.
-- It is a record, main field is optionsRunMode 
data Options =
                Master  CommonOpts      MasterOptions   [String]
        |       Slave   CommonOpts      [String]
        deriving (Show)

data CommonOpts = CommonOpts {
          commonOptsCADFileName         :: Maybe String
        , commonOptsHost                :: String
        , commonOptsPort                :: String
        }
        deriving (Show)

data MasterOptions = MasterOptions {
          masterOptsCrash               :: Bool
        , masterOptsFilename            :: String
        , masterOptsAlignment           :: Int64
        }
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
                        (info (Master <$> commonOptions <*> masterOptions <*> restParser) (fullDesc <> progDesc "run as master on IP:PORT"))
                slave = command "slave"
                        (info (Slave <$> commonOptions <*> restParser) (fullDesc <> progDesc "run as slave on IP:PORT"))
                alignment = option auto (short 'a' <> long "alignment" <> help "file access alignment" <> value requiredAlignmentInItems)
                commonOptions = CommonOpts <$> cadFile <*> ip <*> port
                cadFile = Just <$> strOption (metavar "CAD" <> long "cad" <> help "Filename of cluster architecture description file.")
                        <|> pure Nothing
                masterOptions = MasterOptions <$> crashOpt <*> filenameOpt <*> alignment
                filenameOpt = strOption (metavar "FILE" <> long "filename" <> short 'f' <> help "Filename to read data from." <> value "float_file.txt")
                crashOpt = switch (long "crash" <> short 'c' <> help "make one node to crash.")
                restParser = many (argument str (metavar "ARGS"))
                ip = strOption (metavar "IP" <> long "ip" <> short 'i' <> help "IP address")
                port = strOption (metavar "PORT" <> long "port" <> short 'p' <> help "port for binnding")

-- |An interface to parse command line options.
-- Returns options parsed as data type @Options@.
dnaParseCommandLineOpts :: String -> IO Options
dnaParseCommandLineOpts progName = do
        execParser $ optionsParser progName


dnaParseCommandLineAndRun :: RemoteTable -> String -> (MasterOptions -> Backend -> [NodeId] -> Process ()) -> IO ()
dnaParseCommandLineAndRun remoteTable progName master= do
        options <- dnaParseCommandLineOpts progName
        case options of
                Master (CommonOpts cadFile ip port) masterOptions _args -> do
                        backend <- initializeBackend cadFile ip port remoteTable
                        startMaster backend (master masterOptions backend)
                        liftIO $ threadDelay 100
                Slave (CommonOpts cadFile ip port) _args -> do
                        backend <- initializeBackend cadFile ip port remoteTable
                        startSlave backend
