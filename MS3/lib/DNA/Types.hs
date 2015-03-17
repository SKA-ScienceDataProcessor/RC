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

-- | Command that process is ready
data DoneTask = DoneTask
                deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary DoneTask

data Terminate = Terminate
                 deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary Terminate

-- | Resources allocated to single process. It always have access to
--   node it owns and possibly list of other nodes.
data VirtualCAD = VirtualCAD
    { vcadLoc      :: Location
    , vcadNode     :: NodeId
    , vcadNodePool :: [NodeId]
    } 
    deriving (Show,Eq,Ord,Typeable,Generic)

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

-- | How process crashes should be treated in the group
data GroupType
    = Normal
    | Failout
    deriving (Show,Typeable,Generic)
instance Binary GroupType

-- | ID of group of processes
newtype GroupID = GroupID Int
                deriving (Show,Eq,Ord,Typeable,Binary)

-- | Way to encode
data ActorID = SingleActor ProcessId
             | ActorGroup  GroupID
             deriving (Show,Typeable,Generic)
instance Binary ActorID

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

-- | Map-reduce actor
data MR a
    deriving (Typeable)



----------------------------------------------------------------
-- Shell actors
----------------------------------------------------------------

-- | Shell actor. It's actor which hasn't been connected anywhere.
data Shell a b = Shell
    ActorID     -- ID of actor. Its process or group ID
    (RecvEnd a) -- Protocol for receiving data
    (SendEnd b) -- Protocol for sending data
    deriving (Typeable,Generic)

-- Quadratic number of instances in number of type tags. Sigh
instance (Serializable a, Serializable b) => Binary (Shell (Val     a) (Val     b))
instance (Serializable a, Serializable b) => Binary (Shell (Val     a) (Grp     b))
instance (Serializable a, Serializable b) => Binary (Shell (Val     a) (MR      b))
--
instance (Serializable a, Serializable b) => Binary (Shell (Grp     a) (Val     b))
instance (Serializable a, Serializable b) => Binary (Shell (Grp     a) (Grp     b))
instance (Serializable a, Serializable b) => Binary (Shell (Grp     a) (MR      b))
--
instance (Serializable a, Serializable b) => Binary (Shell (Scatter a) (Val     b))
instance (Serializable a, Serializable b) => Binary (Shell (Scatter a) (Grp     b))
instance (Serializable a, Serializable b) => Binary (Shell (Scatter a) (MR      b))
--
instance (Serializable a, Serializable b) => Binary (Shell (MR      a) (Val     b))
instance (Serializable a, Serializable b) => Binary (Shell (MR      a) (Grp     b))
instance (Serializable a, Serializable b) => Binary (Shell (MR      a) (MR      b))



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
    -- | Actors which reduce output of mappers
    RecvMR :: [(SendPort Int, SendPort (Maybe a))]
           -> RecvEnd (MR a)
    deriving (Typeable)


-- | Description of send end of actor
data SendEnd a where
    -- | Actor sends single value
    SendVal :: SendPort (Dest a)
            -> SendEnd (Val a)
    -- | Actor sends group of values
    SendGrp :: [SendPort (Dest a)]
            -> SendEnd (Grp a)
    -- | Actor sends group of streams
    SendMR  :: [SendPort [SendPort (Maybe a)]]
            -> SendEnd (MR a)
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

instance (Typeable a, Binary a) => Binary (RecvEnd (MR a)) where
    put (RecvMR a) = putWord8 5 >> put a
    get = do
        t <- getWord8
        case t of
          5 -> RecvMR <$> get
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

instance (Typeable a, Binary a) => Binary (SendEnd (MR a)) where
    put (SendMR a) = putWord8 3 >> put a
    get = do
        t <- getWord8
        case t of
          3 -> SendMR <$> get
          _ -> fail "Bad tag"


-- | Destination for actor computation
data Dest a
    = SendLocally (SendPort a)
      -- ^ Send result using using unsafe primitive
    | SendRemote [SendPort a]
      -- ^ Send result using standard primitives
    deriving (Show,Typeable,Generic)

instance Serializable a => Binary (Dest a)
