{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
module DNA.Message (sayDebug, dnaSlaveHandleStart, dnaMasterStartSlave, DnaStart, DnaStarted, DnaFinished(DnaFinished), DnaPidList(DnaPidList)) where

import Text.Printf
--import Control.Distributed.Process.Debug
import Control.Distributed.Process hiding (say)
import Control.Distributed.Process.Platform (resolve)
import qualified Control.Distributed.Process.Platform.Service.SystemLog as Log
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

import DNA.Common (say)

newtype DnaStart = DnaStart ProcessId deriving (Eq, Ord, Show, Typeable, Generic)
newtype DnaStarted = DnaStarted ProcessId deriving (Eq, Ord, Show, Typeable, Generic)
newtype DnaFinished = DnaFinished ProcessId deriving (Eq, Ord, Show, Typeable, Generic)
newtype DnaPidList = DnaPidList [ProcessId] deriving (Eq, Ord, Show, Typeable, Generic)
     
instance Binary DnaStart
instance Binary DnaStarted
instance Binary DnaFinished
instance Binary DnaPidList


sayDebug :: String -> Process ()
sayDebug msg = do
	say msg
{-
	logger <- Log.client
	case logger of
		Nothing -> do
			--liftIO $ putStrLn $ "unable to resolve logger for "++show msg
			log <- whereis "logger"
			case log of
				Just l -> do
					us <- getSelfPid
					send l $ ("<no time>", us, msg)
				Nothing -> error "completely unable to resolve any logging facility!"
			return ()
		Just cl -> do
			r <- resolve cl
			say $ "cl agent "++show r
			Log.debug cl (Log.LogText msg)
-}


-- in: name slavePid out: masterPid
dnaSlaveHandleStart :: String -> ProcessId -> Process (ProcessId)
dnaSlaveHandleStart name slavePid = do
        sayDebug $ printf "[%s] : Waiting for master." name
        (DnaStart masterPid) <- expect     
        sayDebug $ printf "[%s] : DnaStart from master :%s" name (show masterPid)
--        send masterPid (DnaStarted slavePid)
        sayDebug $ printf "[%s] : Started" name
        return masterPid

-- in: name masterPid nodeId  out: slavePid
dnaMasterStartSlave :: String -> ProcessId -> NodeId -> Closure( Process() ) -> Process ProcessId
dnaMasterStartSlave name masterPid nodeId clo = do
        sayDebug $ printf "[Master] : spawning %s on node: %s" name (show nodeId)
        pid <- spawn nodeId clo
--        enableTrace pid
        send pid (DnaStart masterPid)
--        (DnaStarted slavePid) <- expect
        let slavePid = pid
        sayDebug $ printf "[Master] : %s started %s" name (show slavePid)
        return slavePid
