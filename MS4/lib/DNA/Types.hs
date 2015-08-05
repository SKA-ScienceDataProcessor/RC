{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DNA.Types where

import Control.Monad.Reader
import Control.Monad.State.Strict (StateT)
import Control.Monad.Except
import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
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
    { vcadNode     :: NodeInfo
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

-- | Describes where actor should be spawned.
data Location = Remote -- ^ Will be spawned on some other node
              | Local  -- ^ Will be spawned on the same node
              deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary Location

-- | Rank of actor
newtype Rank = Rank Int
             deriving (Show,Eq,Ord,Typeable,Binary)

-- | Size of group of proceesses
newtype GroupSize = GroupSize Int
             deriving (Show,Eq,Ord,Typeable,Binary)

-- | Describes how failures in a group of processes are treated.
data GroupType
    = Normal  -- ^ If a single process in the group fails, it is
              -- treated as if the whole group failed.
    | Failout -- ^ The result of failed processes will be silently
              -- discarded.
    deriving (Show,Typeable,Generic)
instance Binary GroupType

-- | This describes how many nodes we want to allocate either to a
-- single actor process or to the group of processes as whole. We can
-- either request exactly /n/ nodes or a fraction of the total pool of
-- free nodes.
data Res
    = N    Int                -- ^ Fixed number of nodes
    | Frac Double             -- ^ Fraction of nodes. Should lie in /(0,1]/ range.
    deriving (Show,Typeable,Generic)
instance Binary Res

-- | Describes how to divide allocated nodes between worker
-- processes. 'NWorkers' means that we want to divide nodes evenly
-- between /n/ actors. 'NNodes' means that we want allocate no less
-- that /n/ nodes for each actors. In latter case DSL will allocate
-- resources for as many actors as possible given constraints.
data ResGroup
    = NWorkers Int   -- ^ Allocate no less than /n/ workers
    | NNodes   Int   -- ^ Allocate no less than /n/ nodes to each worker
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
newtype Timeout = Timeout AID
                 deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary Timeout

-- | Actor just sent data to following destination
data SentTo = SentTo ProcessId (RecvAddr Recv)
                 deriving (Show,Eq,Typeable,Generic)
instance Binary SentTo

-- | Acknowledgement of data transmission
data AckSend = AckSend
                 deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary AckSend


----------------------------------------------------------------
-- Type tags for shell actors
----------------------------------------------------------------

-- | The actor receives/produces a single value, respectively.
data Val a
    deriving (Typeable)

-- | The actor receives/produces an unordered group of values.
data Grp a
    deriving (Typeable)

-- | Only appears as an input tag. It means that we may want to
-- scatter values to a set of running actors.
data Scatter a
    deriving (Typeable)


----------------------------------------------------------------
-- Shell actors
----------------------------------------------------------------

-- | Address receive port stored as opaque message
data Recv = Recv !SendPortId !Message
          deriving (Show,Typeable,Generic)
instance Eq Recv where
  Recv a _ == Recv b _ = a == b
instance Binary Recv

makeRecv :: (Serializable a) => SendPort a -> Recv
makeRecv ch = Recv (sendPortId ch) (wrapMessage ch)

-- | Address of receive port of an actor. It's parametrized by communication channel type
--
--   Messages are sent via SendPort which is stored as 'Message' to
-- erase type.
data RecvAddr m
    = RcvSimple m
      -- ^ Actor/variable that receive single value
    | RcvReduce m (SendPort Int)
      -- ^ Reduce actor or actors. It's list of ports to send data to
      --   and channels for sending number of values to expect
    | RcvTree [RecvAddr m]
      -- ^ Ports of tree reducer. They have subtly different meaning.
      --   In ordinary collector data is send to each collector. In
      --   tree collector destination is determined by rank.
    | RcvGrp [RecvAddr m]
      -- ^ Group of simple actors
    | RcvCompleted
      -- ^ Address of completed actor
    | RcvFailed
      -- ^ Address of failed actor
    deriving (Show,Eq,Functor,Typeable,Generic)
instance Binary m => Binary (RecvAddr m)

-- | Type of address of actor. It only covers 
data RecvAddrType
    = RcvTySimple
      -- ^ Actor/variable that receive single value
    | RcvTyReduce
      -- ^ Reduce actor or actors
    | RcvTyTree
      -- ^ Tree reduction actor
    deriving (Show,Eq,Typeable,Generic)
instance Binary RecvAddrType


-- | Handle of a running actor or group. Note that we treat actors and
-- groups of actors uniformly here. Shell data type has two type
-- parameters which describe what kind of data actor receives or
-- produces. For example:
--
-- > Shell (InputTag a) (OutputTag b)
--
-- Also both input and output types have tags which describe how many
-- messages data type produces and how this actor could be connected
-- with others. It means that shell receives message(s) of type a and
-- produce message(s) of type b. We support tags 'Val', 'Grp' and
-- 'Scatter'.
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
