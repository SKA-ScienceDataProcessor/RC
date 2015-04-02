module CUDAEx(
   module Foreign.CUDA.Types
 , module Foreign.CUDA.Runtime
 , module Foreign.CUDA.Driver
 , CxDoubleDevPtr
 , DoubleDevPtr
 ) where

import Foreign.CUDA.Driver (
    Fun
  , launchKernel
  , FunParam(..)
  , Module
  , loadFile
  , getFun
  )

import Foreign.CUDA.Runtime hiding (
    Fun
  , launchKernel
  , FunParam(..)
  )

import Foreign.CUDA.Types (Stream)
import Data.Complex

type CxDouble = Complex Double
type CxDoubleDevPtr = DevicePtr CxDouble
type DoubleDevPtr = DevicePtr Double
