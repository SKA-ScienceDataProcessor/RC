{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
module DNA.Types where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Set        as Set
import           Data.Set          (Set)
import GHC.Generics  (Generic)


----------------------------------------------------------------
-- MonadProcess
----------------------------------------------------------------

-- | Monad to which computations in the 'Process' could be lifted
class MonadIO m => MonadProcess m where
    liftP :: Process a -> m a

instance MonadProcess Process where
    liftP = id

instance MonadProcess m => MonadProcess (StateT s m) where
    liftP = lift . liftP

instance MonadProcess m => MonadProcess (ExceptT e m) where
    liftP = lift . liftP

instance MonadProcess m => MonadProcess (ReaderT r m) where
    liftP = lift . liftP



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Rank of actor
newtype Rank = Rank Int
             deriving (Show,Eq,Ord,Typeable,Binary)

-- | Size of group of proceesses
newtype GroupSize = GroupSize Int
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
             deriving (Show,Typeable,Generic,Functor,Foldable,Traversable)

-- | Information about node. It's normally used in the CAD.
data NodeInfo = NodeInfo
    { nodeCP     :: NCP         -- ^ PID of controller process
    , nodeParent :: Maybe NCP   -- ^ PID of parent's controller process
    , nodeID     :: NodeId      -- ^ Node ID
    }
    deriving (Show,Eq,Ord,Typeable,Generic)

data Location = Remote
              | Local
    deriving (Show,Eq,Ord,Typeable,Generic)

-- | Resources allocated to single process. It always have access to
--   node it owns and possibly list of other nodes.
data VirtualCAD = VirtualCAD Location NodeInfo [NodeInfo]
                  deriving (Show,Eq,Ord,Typeable,Generic)

-- | Parameters for ACP process
data ParamACP a = ParamACP
    { acpSelf :: Closure (Process ())
      -- ^ Closure for the DNA.DNA.runACP function. We have to pass it
      --   explicitly since we cannot create it inside @runACP@.
    , acpActorClosure   :: a
      -- ^ Closure for actor to run
    , acpVCAD :: VirtualCAD
      -- ^ Part of cluster allocated to the process
    , acpActor :: ParamActor
      -- ^ Parameters for actor
    }
    deriving (Show,Typeable,Generic)

-- | Parameter send to actor on startup
data ParamActor = ParamActor
    { actorParentACP :: ProcessId
      -- ^ Destination to send channels to.
    , actorRank      :: Rank
      -- ^ Rank of an actor
    , actorGroupSize :: GroupSize
      -- ^ Size of group of actors
    }
    deriving (Show,Typeable,Generic)

-- | Destination for actor computation
data Dest a
    = SendLocally (SendPort a)
      -- ^ Send result using using unsafe primitive
    | SendRemote [SendPort a]
      -- ^ Send result using standard primitives
    deriving (Show,Typeable,Generic)

instance Binary a => Binary (CAD a)
instance Binary NodeInfo
instance Binary VirtualCAD
instance Binary Location
instance Binary a => Binary (ParamACP a)
instance Binary ParamActor
instance Serializable a => Binary (Dest a)



----------------------------------------------------------------
-- Shell actors
----------------------------------------------------------------

-- | Simple shell process
data Shell a b = Shell
    (SendPort a)
    (SendPort (Dest b))
    ACP
    deriving (Show,Typeable,Generic)
instance (Serializable a, Serializable b) => Binary (Shell a b)


-- | Simple collector process
data CollectorShell a b = CollectorShell
     (SendPort a)
     (SendPort Int)
     (SendPort (Dest b))
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
