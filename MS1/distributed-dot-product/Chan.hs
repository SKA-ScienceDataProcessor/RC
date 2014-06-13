{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
-- | Description of distributed program
module Chan where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Except
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Serializable (Serializable)

import qualified Data.Foldable as T
import Data.Binary
import Data.Typeable
import Data.Time.Clock
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map (Map)
import Text.Printf
import GHC.Generics (Generic)
import Prelude hiding (id,(.))



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
-- Chan
----------------------------------------------------------------

-- | Description of distributed program
data Chan a b where
  -- | Execution completed
  Noop :: Chan a b
  -- | Identity
  Id      :: Chan a a
  -- | Category composition
  Compose :: Chan a x -> Chan x b -> Chan a b

  -- | Sum bounded number of values
  FoldSum :: ProcessId          -- ^ Worker process ID
          -> Chan (BoundedV a) (Result b)
  -- | Primitive for calculation of dot product
  DotProduct
    :: Set ProcessId        -- Set of running processes
    -> BoundedProtocol a    -- Protocol for communication with downstream
    -> !Int                 -- Number of completed work
    -> [(Int,Int)]          -- Set of work
    -> Chan X (BoundedV a)


-- | Protocol for working with bounded streams
data BoundedProtocol a = BoundedProtocol ProcessId

instance Category Chan where
  id = Id
  Id   . a    = a
  a    . Id   = a
  Noop . Noop = Noop
  a    . b    = Compose b a


