{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module DNA.Types where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import qualified Data.Set        as Set
import           Data.Set          (Set)
import GHC.Generics  (Generic)


-- | Newtype wrapper for sending parent process
newtype Parent = Parent ProcessId
              deriving (Show,Eq,Typeable,Binary)

-- | Parameters for a subprocess. If process require more than one
--   parameter it's sent as tuple if it doesn't require parameters ()
--   is sent.
newtype Param a = Param a
                  deriving (Show,Eq,Typeable,Binary)

-- | Rank of actor
newtype Rank = Rank Int
             deriving (Show,Eq,Ord,Typeable,Binary)

-- | ID of group of processes
newtype GroupID = GroupID Int
                deriving (Show,Eq,Ord,Typeable,Binary)

-- | ID of actor
newtype ActorID = ActorID Int
                deriving (Show,Eq,Ord,Typeable,Binary)

-- | ID of resourses
newtype Resources = Resources Int
                  deriving (Show,Eq,Ord,Typeable,Binary)

-- | Tag for
data Completed = Completed
                deriving (Show,Eq,Ord,Typeable,Generic)

instance Binary Completed


-- | Command for ACP to terminate
data Terminate = Terminate
                deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary Terminate



----------------------------------------------------------------
-- DNA data types
----------------------------------------------------------------

-- | Handle for node controlling process
newtype NCP = NCP { ncpPID :: ProcessId }
             deriving (Show,Eq,Ord,Typeable,Generic,Binary)

-- | Handle for actor controlling process
newtype ACP = ACP { acpPID :: ProcessId }
             deriving (Show,Eq,Ord,Typeable,Generic,Binary)



----------------------------------------------------------------
-- CAD & Node information
----------------------------------------------------------------

-- | Cluster architecture description. Nodes are arranged into rose
--   tree and it's polymorphic in
data CAD a = CAD a [CAD a]
             deriving (Show,Typeable,Generic,Functor)

-- | Information about node. It's normally used in the CAD.
data NodeInfo = NodeInfo
    { nodeCP     :: NCP         -- ^ PID of controller process
    , nodeParent :: Maybe NCP   -- ^ PID of parent's controller process
    , nodeID     :: NodeId      -- ^ Node ID
    , loggerProc :: ProcessId
      -- ^ Port for sending thread local data
    }
    deriving (Show,Eq,Ord,Typeable,Generic)

data Location = Remote
              | Local
    deriving (Show,Eq,Ord,Typeable,Generic)

-- | Resources allocated to single process. It always have access to
--   node it owns and possibly list of other nodes.
data VirtualCAD = VirtualCAD Location NodeInfo [NodeInfo]
                  deriving (Show,Eq,Ord,Typeable,Generic)

instance Binary a => Binary (CAD a)
instance Binary NodeInfo
instance Binary VirtualCAD
instance Binary Location



-- | Simple shell process
data Shell a b = Shell
    (SendPort a)
    (SendPort [SendPort b])
    ACP
    deriving (Show,Typeable,Generic)
instance (Serializable a, Serializable b) => Binary (Shell a b)


-- | Simple collector process
data CollectorShell a b = CollectorShell
     (SendPort a)
     (SendPort (Maybe Int))
     (SendPort [SendPort b])
     ACP
     deriving (Show,Typeable,Generic)
instance (Serializable a, Serializable b) => Binary (CollectorShell a b)



-- | Group of shell processes
data ShellGroup a b = ShellGroup GroupID [Shell a b]
                    deriving (Show,Typeable,Generic)
instance (Serializable a, Serializable b) => Binary (ShellGroup a b)



-- | Group of collector processes
data GroupCollect a b = GroupCollect GroupID [CollectorShell a b]
                    deriving (Show,Typeable,Generic)
instance (Serializable a, Serializable b) => Binary (GroupCollect a b)



----------------------------------------------------------------
-- Destinations
----------------------------------------------------------------

-- | Destination for result of actor computations
data Dest a = Dest (SendPort a) (SendPort ())

-- | Destination for result of group of actors computation
data DestGrp a = DestGrp (SendPort a) (SendPort (Maybe Int))
