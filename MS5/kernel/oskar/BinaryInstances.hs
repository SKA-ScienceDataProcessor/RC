{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    MagicHash
  #-}

module BinaryInstances where

import GHC.Prim
import GHC.Ptr
import GHC.Types
import Data.Binary
import Foreign.C.Types

-- NOTE: addr2Int# and int2Addr# are strongly deprecated

instance Binary (Ptr a) where
  put (Ptr addr#) = put (I# (addr2Int# addr#))
  get = fmap i2a get
    where i2a (I# i#) = Ptr (int2Addr# i#)

instance Binary CDouble where
  put (CDouble d) = put d
  get = fmap CDouble get
