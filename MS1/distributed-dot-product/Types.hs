{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)


----------------------------------------------------------------
-- Types
----------------------------------------------------------------

-- | Final result of computation
newtype Result   a = Result   a deriving (Show,Typeable,Binary)

-- | Finite stream of values
newtype BoundedV a = BoundedV a deriving (Show,Typeable,Binary)

-- | Number of values
newtype Count    a = Count    a deriving (Show,Typeable,Binary)

-- | Message from worker process saying that it's idle and request
--   more work
newtype Idle = Idle ProcessId
             deriving (Show,Typeable,Generic,Binary)

-- | Uninhabited data type
data X



----------------------------------------------------------------
-- Communication protocols
----------------------------------------------------------------

-- | Communications with master process
data MasterProtocol = MasterProtocol (SendPort String) ProcessId
                    deriving (Show,Typeable,Generic)
instance Binary MasterProtocol

logMsg :: MasterProtocol -> String -> Process ()
logMsg (MasterProtocol ch _) = sendChan ch

idle :: MasterProtocol -> Process ()
idle (MasterProtocol _ pid) = send pid . Idle =<< getSelfPid


-- | Protocol for bounded streams
newtype BoundedProtocol a = BoundedProtocol ProcessId
                          deriving (Show,Typeable,Binary)

sendBoundedStream :: (Serializable a) => BoundedProtocol a -> a -> Process ()
sendBoundedStream (BoundedProtocol pid) a
  = send pid (BoundedV a)

sendBoundedCount :: BoundedProtocol a -> Int -> Process ()
sendBoundedCount (BoundedProtocol pid) n
  = send pid (Count n)


-- | Protocol for sending result of computation
newtype ResultProtocol a = ResultProtocol ProcessId
                         deriving (Show,Typeable,Binary)

sendResult :: (Serializable a) => ResultProtocol a -> a -> Process ()
sendResult (ResultProtocol pid) a
  = send pid (Result a)
