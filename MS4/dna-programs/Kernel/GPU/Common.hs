{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    TypeOperators
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  , StandaloneDeriving
  , DeriveDataTypeable
  , BangPatterns
  #-}

module Kernel.GPU.Common (
   module Foreign.CUDA.Runtime
 , CxDouble
 , CxDoubleDevPtr
 , CxDoubleHostPtr
 , DoubleDevPtr
 , DevicePtr(..)
 , HostPtr(..)
 , Stream
 , launchKernel
 , mapArgs
 ) where

import Foreign.CUDA.Runtime hiding (launchKernel)
import Foreign.CUDA.Runtime.Stream
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import Data.Int
import Data.Complex
import Data.Typeable

import Vector

type CxDouble = Complex Double
type CxDoubleDevPtr = DevicePtr CxDouble
type CxDoubleHostPtr = HostPtr CxDouble
type DoubleDevPtr = DevicePtr Double

deriving instance Typeable DevicePtr
deriving instance Typeable HostPtr

foreign import ccall unsafe __cudaConfigureCall ::
      Int -> Int -> Int ->
      Int -> Int -> Int ->
      Int64 -> Ptr () -> IO CInt

{-# INLINEABLE launchKernel #-}
launchKernel
    :: Fun              -- ^ Device function symbol
    -> (Int,Int,Int)        -- ^ grid dimensions
    -> (Int,Int,Int)    -- ^ thread block shape
    -> Int64            -- ^ shared memory per block (bytes)
    -> Maybe Stream     -- ^ (optional) execution stream
    -> [FunParam]
    -> IO ()
launchKernel !fn (!gx,!gy,!gz) (!bx,!by,!bz) !sm !mst !args = do
  r <- __cudaConfigureCall gx gy gz bx by bz sm (useStream $ maybe defaultStream id mst)
  nothingIfOk $ toEnum $ fromIntegral r
  setParams args
  launch fn

class FP a where
  funp :: a -> FunParam

instance FP Int where funp i = VArg (fromIntegral i :: Int32)
instance FP Float where funp = FArg
instance FP Double where funp = DArg
instance FP CxDouble where funp = VArg
instance FP (DevicePtr a) where funp = VArg
instance FP (Vector a) where
  funp (DeviceVector _ p) = VArg p
  funp _other             = error "Attempted to pass non-device vector to CUDA kernel!"

class ArgMapper a where
  mapArgs :: a
  mapArgs = mapArgs0 []
  mapArgs0 :: [FunParam] -> a

instance ArgMapper [FunParam] where
  mapArgs0 acc = reverse acc

instance (FP a, ArgMapper r) => ArgMapper (a -> r) where
  mapArgs0 acc v = mapArgs0 (funp v : acc)
