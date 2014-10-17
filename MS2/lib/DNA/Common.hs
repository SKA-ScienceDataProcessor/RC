-- |Common.hs
--
-- Common functions for logging, etc.
--
-- Copyright (C) 2014 Braam Research, LLC.

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module DNA.Common(
	  startLogger
	, say
        , startTracing
	) where

import Control.Monad
import qualified Control.Distributed.Process as P
import qualified Control.Distributed.Process.Debug as D
import Control.Distributed.Process.Platform (resolve)
import qualified Control.Distributed.Process.Platform.Service.SystemLog as Log

import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

-- |Message to logger.
data Log = Log String deriving (Show,Typeable,Generic)
instance Binary Log where

-- |Common logger process name.
loggerProcessName :: String
loggerProcessName = "ddp-logger"


-- |Start the logger. Run it on master node.
startLogger :: [P.NodeId] -> P.Process ()
startLogger peers = do
	pid <- P.getSelfPid
	P.spawnLocal $ loggerProcess peers pid
	() <- P.expect
	return ()


-- |Logger process - register itself and waits for Log messages.
loggerProcess :: [P.NodeId] -> P.ProcessId ->  P.Process ()
loggerProcess peers starter = do
	id <- P.getSelfPid
	P.register loggerProcessName id
	forM_ peers $ \n -> P.registerRemoteAsync n loggerProcessName id
	P.send starter ()
	forever $ do
		Log s <- P.expect
		P.liftIO $ putStrLn s

-- |Send log messages to logger process.
say :: String -> P.Process ()
say s = do
	logger <- Log.client
	case logger of
		Nothing -> do
			log <- P.whereis "logger"
			case log of
				Just l -> do
					us <- P.getSelfPid
					P.send l $ ("<no time>", us, s)
				Nothing -> error "completely unable to resolve any logging facility!"
			return ()
		Just cl -> do
			r <- resolve cl
			Log.debug cl (Log.LogText s)

startTracing :: [P.NodeId] -> P.Process ()
startTracing peers = do
	-- enable tracing after all is set up.
	forM_ peers $ \peer -> do
		D.startTraceRelay peer
		D.setTraceFlags $ D.TraceFlags {
			  D.traceSpawned = Nothing
			, D.traceDied = Nothing
			, D.traceRegistered = Nothing
			, D.traceUnregistered = Nothing
			, D.traceSend = Just D.TraceAll
			, D.traceRecv = Just D.TraceAll
			, D.traceNodes = True
			, D.traceConnections = True
			}
