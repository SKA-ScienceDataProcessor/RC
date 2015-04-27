{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DNA.Types where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict (StateT)
import Control.Monad.Except
import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary   (Binary(..))
import Data.Typeable (Typeable)
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
-- Other types
----------------------------------------------------------------

-- | Resources allocated to single process. It always have access to
--   node it owns and possibly list of other nodes.
data VirtualCAD = VirtualCAD
    { vcadLoc      :: Location
    , vcadNode     :: NodeInfo
    , vcadNodePool :: [NodeInfo]
    } 
    deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary VirtualCAD

-- | Information about node
data NodeInfo = NodeInfo
  { nodeId :: NodeId
  }
  deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary NodeInfo

data Location = Remote
              | Local
              deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary Location

-- | Rank of actor
newtype Rank = Rank Int
             deriving (Show,Eq,Ord,Typeable,Binary)

-- | Size of group of proceesses
newtype GroupSize = GroupSize Int
             deriving (Show,Eq,Ord,Typeable,Binary)

-- | What part of process pool is to use
data Res
    = N    Int                -- ^ Fixed number of nodes
    | Frac Double             -- ^ Fraction of nodes
    deriving (Show,Typeable,Generic)
instance Binary Res

-- | What part of process pool is to use
data ResGroup
    = NWorkers Int   -- ^ Allocate no less than N workers
    | NNodes   Int   -- ^ Allocate no less than N nodes to each worker
    deriving (Show,Typeable,Generic)
instance Binary ResGroup

-- | ID of group of processes
newtype AID = AID Int
            deriving (Show,Eq,Ord,Typeable,Binary)

-- | ID of actor-local variable
newtype VID = VID Int
            deriving (Show,Eq,Ord,Typeable,Binary)


----------------------------------------------------------------
-- Control messages
----------------------------------------------------------------

-- | Command that process is ready
data DoneTask = DoneTask
                deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary DoneTask

-- | Terminate process
newtype Terminate = Terminate String
                 deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary Terminate

-- | Actor execution time exceeded quota
data TimeOut = TimeOut AID
                 deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary TimeOut



----------------------------------------------------------------
-- Type tags for shell actors
----------------------------------------------------------------

-- | Tag for single value.
--
--    * Receive: actor accept single value as parameter
--    * Send: actor produces single value as result
data Val a
    deriving (Typeable)

-- | Tag for unordered group of values.
--
--    * Receive: ???
--    * Send: actor produces set of messages in arbitrary order.
data Grp a
    deriving (Typeable)

-- | Tags for ordered set of values
data Scatter a
    deriving (Typeable)

{-
-- | Map-reduce actor
data MR a
    deriving (Typeable)
-}


----------------------------------------------------------------
-- Shell actors
----------------------------------------------------------------

-- | Address of receive port of an actor.
--
--   Messages are sent via SendPort which is stored as 'Message' to
--   erase type.
data RecvAddr
    = RcvSimple Message
      -- ^ Actor/variable that receive single value
    | RcvReduce [(Message,SendPort Int)]
      -- ^ Reduce actor or actors
    | RcvGrp [Message]
      -- ^ Group of simple actors
    deriving (Show,Typeable,Generic)
instance Binary RecvAddr


-- | Spawned actor.
--
--   Internally it's just phantom typed wrapped over AID. All
--   necessary data is stored in the

newtype Shell a b = Shell AID
                    deriving (Typeable,Generic,Binary)

-- | Destination for actor computation
data Dest a
    = SendLocally (SendPort a)
      -- ^ Send result using using unsafe primitive
    | SendRemote [SendPort a]
      -- ^ Send result using standard primitives
    deriving (Show,Typeable,Generic)
instance Serializable a => Binary (Dest a)
