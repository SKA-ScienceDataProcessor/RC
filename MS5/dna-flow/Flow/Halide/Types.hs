{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Data types for Halide wrappers
module Flow.Halide.Types where


import Data.Int
import Data.Vector.HFixed.Class  (Fn)
import Foreign.C
import Foreign.Ptr

import Flow.Halide.BufferT

import Flow.Internal
import Flow.Vector


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Newtype wrapper for scalar value. Only present to please type
--   checker.
newtype Scalar a = Scalar a
                   deriving (Show)

-- | Representation of array. It's GC controlled buffer in memory and 
data Array dim a = Array
  { arrayExtent :: dim       -- ^ Array shape
  , arrayBuffer :: Vector a  -- ^ Pointer to buffer
  }

-- | Type which describes dimension of value
type family   Extent a
type instance Extent (Array dim a) = dim
type instance Extent (Scalar a)    = ()


----------------------------------------------------------------
-- Dynamic typing
----------------------------------------------------------------

-- | Wrapper for scalar values
data ScalarVal
  = Double !Double
  | Float  !Float
  | Int32  !Int32
  deriving (Show)

-- | Wrapper for any known type
data Box
  = Number    !ScalarVal
    -- ^ Any scalar data type
  | SomeArray !ScalarVal [(Int32,Int32)] (Vector ())
    -- ^ Some array. ScalarVal holds type of an array
  | Extent [(Int32,Int32)]
  | Unit

----------------------------------------------------------------
-- FFI types
----------------------------------------------------------------

-- |
-- Newtype wrapper for halide kernel. It's needed to preserve
-- injectivity of type. Note that it could be used as:
--
-- > foreign import ccall "kern_generate_f"
-- >   kern_generate_f :: Kernel '[Int32, Array Dim1 Float] (Array Dim1 Float)
--
-- Output is always(?) represented as an array even if we return a scalar.
newtype HalideKernel xs a = HalideKernel (Fn (KernelCParams xs) (Ptr BufferT -> IO CInt))


-- | Types for C parameters corresponding to haskell values
type family KernelCParams (xs :: [*])  :: [*]

type instance KernelCParams '[]                        = '[]
type instance KernelCParams (Array (d1 :. d2) x ': xs) = Ptr BufferT ': KernelCParams xs
type instance KernelCParams (Array  Z         x ': xs) = x           ': KernelCParams xs
type instance KernelCParams (Scalar x           ': xs) = x           ': KernelCParams xs
