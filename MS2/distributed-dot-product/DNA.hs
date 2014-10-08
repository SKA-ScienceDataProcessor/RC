{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Common moulde for DNA
module DNA where

import Control.Monad
import Control.Distributed.Process hiding (say)
import Control.Distributed.Process.Closure
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe
import Control.Distributed.Process.Serializable (Serializable)

import Data.Binary   (Binary)
import Data.Int
import Data.Typeable (Typeable)
import qualified Data.Vector.Storable as S
import GHC.Generics  (Generic)

import DNA.Logging
import DNA.Run

----------------------------------------------------------------
-- Data types and helpers
----------------------------------------------------------------

-- | Parameters for a subprocess. If process require more than one
--   parameter it's sent as tuple if it doesn't require parameters ()
--   is sent.
newtype Param a = Param a
                  deriving (Show,Eq,Typeable,Binary)


-- | Normally subprocess will only return value once. Fork* function
--   return promise which is hadle for obtaining result of
--   computation.
--
--   We use cloud haskell channels for commutincations instead of
--   send\/expect because this way we cannot mix up messages from
--   different sources.
--
--   FIXME: If we await more than once we await more than once we wll
--          deadlock and we have no way to detect such behaviour. Nor
--          we can detect if we do not await for promise. In this case
--          child process will be termintaed forcefully when parent
--          dies.
data Promise a = Promise ProcessId (ReceivePort a)


-- | Await result from promise. Function will block.
--
--   This function will as well receive messages about termination of
--   monitored processes.
--
--   FIXME: Currently we terminate when child dies abnormally. But for
--          some processes we may want to use different strategy.
--          We need to keep some data about known childs etc.
await :: Serializable a => Promise a -> Process a
await p@(Promise _ ch) = do
    -- Here we wait for either value from channel or for message that
    -- child died.
    receiveWait
        [ matchChan ch return
        , match $ \(ProcessMonitorNotification _ _ reason) ->
            case reason of
              DiedNormal -> await p
              _          -> terminate
        ]

-- | Cluster architecture description. Currently it's simply list of
--   nodes process can use.
newtype CAD = CAD [NodeId]
              deriving (Show,Eq,Typeable,Binary)



----------------------------------------------------------------
-- Starting of child processes
----------------------------------------------------------------


-- | Start process.
startProcess
    :: (Serializable a, Serializable b)
    => ([NodeId] -> a -> Process b)
    -> Process ()   
startProcess action = do
    sendCh  <- expect
    nodes   <- expect
    Param a <- expect
    b       <- action nodes a
    sendChan sendCh b

-- | Fork process on local node
forkLocal :: (Serializable a, Serializable b)
          => [NodeId]           -- ^ List of nodes process allowed to use
          -> Process ()         -- ^ Process command
          -> a                  -- ^ Parameters to process
          -> Process (Promise b)
forkLocal nodes child a = do
    me  <- getSelfPid
    pid <- spawnLocal $ link me >> child
    (chSend,chRecv) <- newChan
    send pid chSend
    send pid (CAD nodes)
    send pid (Param a)
    return $ Promise me chRecv



-- | Fork process on remote node
forkRemote :: (Serializable a, Serializable b)
           => [NodeId]             -- ^ List of nodes process allowed to use
           -> NodeId               -- ^ Node to spawn on
           -> Closure (Process ()) -- ^ Sub process command
           -> a                    -- ^ Parameters sent to process
           -> Process (Promise b)
forkRemote nodes nid child a = do
    me <- getSelfPid
    (pid,_) <- spawnSupervised nid child
    (chSend,chRecv) <- newChan
    send pid chSend
    send pid (CAD nodes)
    send pid (Param a)
    return $ Promise me chRecv
