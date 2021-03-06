{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
      StandaloneDeriving
    , DeriveDataTypeable
    , BangPatterns
  #-}

module CUDAEx(
   module Foreign.CUDA.Runtime
 , CxDouble
 , CxDoubleDevPtr
 , CxDoubleHostPtr
 , DoubleDevPtr
 , DevicePtr(..)
 , HostPtr(..)
 , Stream
 , launchKernel
 ) where

import Foreign.CUDA.Runtime hiding (launchKernel)
import Foreign.CUDA.Runtime.Stream
import Foreign.C
import Foreign.Ptr
import Data.Int
import Data.Complex
import Data.Typeable

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
