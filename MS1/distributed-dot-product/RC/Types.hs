{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Common types
module RC.Types where

import Control.Distributed.Process
import Data.Binary  (Binary)
import Data.Data    (Typeable)
import GHC.Generics (Generic)


-- | Final result of computation
newtype Result a = Result a
                 deriving (Show,Typeable,Binary)

-- | Message from worker process saying that it's idle and request
--   more work
newtype Idle = Idle ProcessId
             deriving (Show,Typeable,Generic,Binary)

-- | Node is released and could be used by other actors.
newtype ReleaseNode = ReleaseNode NodeId
             deriving (Show,Typeable,Generic,Binary)

-- | Send more nodes to worker
newtype LeaseNodes = LeaseNodes [NodeId]
             deriving (Show,Typeable,Generic,Binary)

-- | ID of work. It's used to distinguish between different work
-- batches sent to workers.
newtype WorkID = WorkID Int
                 deriving (Show,Typeable,Generic,Binary,Eq,Ord,Enum)

data Batch a = Batch WorkID a
               deriving (Show,Typeable,Generic)
instance Binary a => Binary (Batch a)

data WorkResult a = WorkResult WorkID a
               deriving (Show,Typeable,Generic)
instance Binary a => Binary (WorkResult a)



-- | How many nodes could use worker process
data NodeRequest
  = NoNeed                      -- ^ Process doesn't need any more nodes
  | UpTo Int                    -- ^ Process could use up to N nodes
  | ManyNodes                   -- ^ Process will use as many nodes as
                                --   possible.
  deriving (Show,Typeable,Generic)
instance Binary NodeRequest
