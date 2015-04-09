{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
      StandaloneDeriving
    , DeriveDataTypeable
  #-}

module CUDAEx(
   module Foreign.CUDA.Types
 , module Foreign.CUDA.Runtime
 , module Foreign.CUDA.Driver
 , CxDouble
 , CxDoubleDevPtr
 , CxDoubleHostPtr
 , DoubleDevPtr
 , DevicePtr(..)
 , HostPtr(..)
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
import Data.Typeable

type CxDouble = Complex Double
type CxDoubleDevPtr = DevicePtr CxDouble
type CxDoubleHostPtr = HostPtr CxDouble
type DoubleDevPtr = DevicePtr Double

deriving instance Typeable DevicePtr
deriving instance Typeable HostPtr
