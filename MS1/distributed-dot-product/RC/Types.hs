{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Common types
module RC.Types (
    Idle(..)
  ) where

import Control.Distributed.Process
import Data.Binary  (Binary)
import Data.Data    (Typeable)
import GHC.Generics (Generic)

-- | Message from worker process saying that it's idle and request
--   more work
newtype Idle = Idle ProcessId
             deriving (Show,Typeable,Generic,Binary)
