{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    MagicHash
  #-}

module BinaryInstances where

import GHC.Prim
import GHC.Ptr
import GHC.Types
import Data.Binary
import Foreign.CUDA.Ptr

-- NOTE: addr2Int# and int2Addr# are strongly deprecated

instance Binary (Ptr a) where
  put (Ptr addr#) = put (I# (addr2Int# addr#))
  get = fmap i2a get
    where i2a (I# i#) = Ptr (int2Addr# i#)
  
instance Binary (FunPtr a) where
  put = put . castFunPtrToPtr
  get = fmap castPtrToFunPtr get

instance Binary (DevicePtr a) where
  put (DevicePtr (Ptr addr#)) = put (I# (addr2Int# addr#))
  get = fmap i2a get
    where i2a (I# i#) = DevicePtr (Ptr (int2Addr# i#))
