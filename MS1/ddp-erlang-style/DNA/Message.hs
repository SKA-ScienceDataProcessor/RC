{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
module DNA.Message (dnaSlaveHandleStart, dnaMasterStartSlave, DnaStart, DnaStarted, DnaFinished(DnaFinished), DnaPidList(DnaPidList)) where

import Text.Printf
import Control.Distributed.Process.Debug
import Control.Distributed.Process
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

newtype DnaStart = DnaStart ProcessId deriving (Eq, Ord, Show, Typeable, Generic)
newtype DnaStarted = DnaStarted ProcessId deriving (Eq, Ord, Show, Typeable, Generic)
newtype DnaFinished = DnaFinished ProcessId deriving (Eq, Ord, Show, Typeable, Generic)
newtype DnaPidList = DnaPidList [ProcessId] deriving (Eq, Ord, Show, Typeable, Generic)
     
instance Binary DnaStart
instance Binary DnaStarted
instance Binary DnaFinished
instance Binary DnaPidList

-- in: name slavePid out: masterPid
dnaSlaveHandleStart :: String -> ProcessId -> Process (ProcessId)
dnaSlaveHandleStart name slavePid = do
       	say $ printf "[%s] : Waiting for master." name
	(DnaStart masterPid) <- expect     
  	say $ printf "[%s] : DnaStart from master :%s" name (show masterPid)
  	send masterPid (DnaStarted slavePid)
  	say $ printf "[%s] : Started" name
        return masterPid

-- in: name masterPid nodeId  out: slavePid
dnaMasterStartSlave :: String -> ProcessId -> NodeId -> Closure( Process() ) -> Process ProcessId
dnaMasterStartSlave name masterPid nodeId clo = do
 	say $ printf "[Master] : spawning %s on node: %s" name (show nodeId)
        pid <- spawn nodeId clo
        enableTrace pid
        send pid (DnaStart masterPid)
        (DnaStarted slavePid) <- expect
 	say $ printf "[Master] : %s started %s" name (show slavePid)
        return slavePid
