{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
module DNA.Types where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State (StateT)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary   (Binary(..))
import Data.Typeable (Typeable)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
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
    | SendPool
      -- ^ Send value to a pool of worker processes
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


-- | Way to encode
data ActorACP = SingleActor ACP
              | ActorGroup  GroupID
    deriving (Show,Typeable,Generic)
instance Binary ActorACP

-- | Shell actor. It's actor which hasn't been connected anywhere.
data Shell a b = Shell
     ActorACP
    (RecvEnd a)
    (SendEnd b)
    deriving (Typeable,Generic)

-- Quadratic number of instances in number of type tags. Sigh
instance (Serializable a, Serializable b) => Binary (Shell (Val     a) (Val     b))
instance (Serializable a, Serializable b) => Binary (Shell (Val     a) (Grp     b))
-- instance (Serializable a, Serializable b) => Binary (Shell (Val     a) (Scatter b))
instance (Serializable a, Serializable b) => Binary (Shell (Grp     a) (Val     b))
instance (Serializable a, Serializable b) => Binary (Shell (Grp     a) (Grp     b))
-- instance (Serializable a, Serializable b) => Binary (Shell (Grp     a) (Scatter b))
instance (Serializable a, Serializable b) => Binary (Shell (Scatter a) (Val     b))
instance (Serializable a, Serializable b) => Binary (Shell (Scatter a) (Grp     b))
-- instance (Serializable a, Serializable b) => Binary (Shell (Scatter a) (Scatter b))



-- | Describe how actor accepts 
data RecvEnd a where
    -- | Actor receives single value
    RecvVal :: SendPort a
            -> RecvEnd (Val a)
    -- | Actor receives group of values
    RecvGrp :: [SendPort a]
            -> RecvEnd (Scatter a)
    -- | Same value is broadcasted to all actors in group
    RecvBroadcast :: RecvEnd (Scatter a)
                  -> RecvEnd (Val a)
    -- | Actor(s) which reduces set of values
    RecvReduce :: [(SendPort Int,SendPort a)]
               -> RecvEnd (Grp a)
    deriving (Typeable)


-- | Description of send end of actor
data SendEnd a where
    -- | Actor sends single value
    SendVal :: SendPort (Dest a)
            -> SendEnd (Val a)
    -- | Actor sends group of values
    SendGrp :: [SendPort (Dest a)]
            -> SendEnd (Grp a)
    deriving (Typeable)

instance (Typeable a, Binary a) => Binary (RecvEnd (Val a)) where
    put (RecvVal       p) = putWord8 1 >> put p
    put (RecvBroadcast p) = putWord8 3 >> put p
    get = do
        t <- getWord8
        case t of
          1 -> RecvVal <$> get
          3 -> RecvBroadcast <$> get
          _ -> fail "Bad tag"

instance (Typeable a, Binary a) => Binary (RecvEnd (Scatter a)) where
    put (RecvGrp    p  ) = putWord8 2 >> put p
    get = do
        t <- getWord8
        case t of
          2 -> RecvGrp <$> get
          _ -> fail "Bad tag"

instance (Typeable a, Binary a) => Binary (RecvEnd (Grp a)) where
    put (RecvReduce a) = putWord8 4 >> put a
    get = do
        t <- getWord8
        case t of
          4 -> RecvReduce <$> get
          _ -> fail "Bad tag"


instance (Typeable a, Binary a) => Binary (SendEnd (Val a)) where
    put (SendVal ch) = putWord8 1 >> put ch
    get = do
        t <- getWord8
        case t of
          1 -> SendVal <$> get
          _ -> fail "Bad tag"

instance (Typeable a, Binary a) => Binary (SendEnd (Grp a)) where
    put (SendGrp ch) = putWord8 2 >> put ch
    get = do
        t <- getWord8
        case t of
          2 -> SendGrp <$> get
          _ -> fail "Bad tag"
