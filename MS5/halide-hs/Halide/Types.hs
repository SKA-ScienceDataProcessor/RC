{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Data types for Halide wrappers
module Halide.Types where


import Control.Applicative
import Control.Monad
import Data.Int
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable

import Halide.BufferT



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Newtype wrapper for scalar value. Only present to please type
--   checker.
newtype Scalar a = Scalar a
                   deriving (Show)

-- | Representation of array
data Array dim a = Array
  { arrayExtent :: dim           -- ^ Array shape
  , arrayBuffer :: ForeignPtr a  -- ^ Pointer to buffer
  }

data Z = Z
       deriving (Show)

data (:.) a b = a :. b
              deriving (Show)
infixr :.

type Dim1 = (Int32,Int32) :. Z

----------------------------------------------------------------
-- Manual marshaling
----------------------------------------------------------------

-- | Type class for scalars known by Halide
class Storable a => HalideScalar a where

instance HalideScalar Int32
instance HalideScalar Float
instance HalideScalar Double

-- | Type class for marshalling array of different dimensions
class MarshalArray dim where
  -- | Copy array information to the buffer_t for passing
  wrapArray  :: HalideScalar a => Array dim a -> IO BufferT
  -- | Allocate new array. Memory content is not initialized
  allocArray :: HalideScalar a => dim -> IO (Array dim a)
  allocArray dim = do
    arr <- mallocForeignPtrArray $ nOfElements dim
    return $ Array dim arr
  -- | Full array size
  nOfElements :: dim -> Int

withScalarResult :: forall a b. HalideScalar a => (Ptr BufferT -> IO b) -> IO (Scalar a,b)
withScalarResult action = do
  buf <- newBufferT
  setElemSize buf $ sizeOf (undefined :: a)
  setBufferStride  buf 1 0 0 0
  setBufferExtents buf 1 0 0 0
  alloca $ \ptr -> do
    setHostPtr buf (castPtr ptr)
    r <- withBufferT buf action
    a <- peek ptr
    return (Scalar a, r)



instance MarshalArray ((Int32,Int32) :. Z) where
  wrapArray (Array ((off,size) :. Z) arr) = do
    buf <- newBufferT
    setElemSize buf $ sizeOfVal arr
    setBufferStride  buf    1 0 0 0
    setBufferMin     buf  off 0 0 0
    setBufferExtents buf size 0 0 0
    withForeignPtr arr $ setHostPtr buf . castPtr
    return buf
  nOfElements ((_,n) :. Z) = fromIntegral n

sizeOfVal :: forall p a. HalideScalar a => p a -> Int
sizeOfVal _ = sizeOf (undefined :: a)



----------------------------------------------------------------
-- FFI types
----------------------------------------------------------------

-- | Type signature for function which takes list of parameters xs and
--   generate result of type a
type family KernelCSig (xs :: [*]) (a :: *)

-- | Output is always(?) represented as an array even if we return a
--   scalar.
type instance KernelCSig '[]                 a = (Ptr BufferT -> IO CInt)

type instance KernelCSig (Array dim x ': xs) a = Ptr BufferT -> KernelCSig xs a
type instance KernelCSig (Scalar x    ': xs) a = x           -> KernelCSig xs a
