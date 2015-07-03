{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Vector
  ( Vector(..)
  , vectorSize
  , nullVector
  , castVector
  , offsetVector
  , peekVector, pokeVector
  -- * Memory Management
  , allocCVector, allocHostVector, allocDeviceVector
  , freeVector
  -- * Conversion
  , toCVector, toHostVector, toDeviceVector
  , dupCVector, dupHostVector, dupDeviceVector
  ) where

import Data.Binary   (Binary(..))
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (advancePtr)
import Foreign.Marshal.Utils

import Foreign.CUDA.Ptr as C
import Foreign.CUDA.Runtime as CUDA

import Foreign.Storable

import GHC.Generics (Generic)


-- | Vector type. Depending on the requirements of the kernel that
-- uses it, it might have a different underlying pointer type.
data Vector a
  = CVector !Int (Ptr a)
  | HostVector !Int (HostPtr a)
  | DeviceVector !Int (DevicePtr a)
  deriving (Show, Typeable, Generic)
-- FIXME: No sane binary  instance but we need to pass this data type around
instance Binary (Vector a) where
    get = undefined
    put = undefined

-- | Returns the number of elements a vector has. Note that this will
-- return @0@ for the result of "offsetVector".
vectorSize :: Vector a -> Int
vectorSize (CVector n _) = n
vectorSize (HostVector n _) = n
vectorSize (DeviceVector n _) = n

-- | A vector carrying no data, pointing nowhere
nullVector :: Vector a
nullVector = CVector 0 nullPtr

-- | Cast a vector to a different element type. Moderately evil.
castVector :: Vector a -> Vector b
castVector (CVector n p) = CVector n $ castPtr p
castVector (HostVector n p) = HostVector n $ HostPtr $ castPtr $ useHostPtr p
castVector (DeviceVector n p) = DeviceVector n $ DevicePtr $ castPtr $ useDevicePtr p

-- | Make an at-offset vector. Note that this vector will only remain
-- valid as long as the original vector data isn't free.
offsetVector :: Storable a => Vector a -> Int -> Vector a
offsetVector (CVector _ p) off = CVector 0 $ p `advancePtr` off
offsetVector (HostVector _ p) off = HostVector 0 $ p `advanceHostPtr` off
offsetVector (DeviceVector _ p) off = DeviceVector 0 $ p `advanceDevPtr` off

-- | Read an element from the vector
peekVector :: Storable a => Vector a -> Int -> IO a
peekVector (CVector _ p)    off = peekElemOff p off
peekVector (HostVector _ p) off = peekElemOff (useHostPtr p) off
peekVector (DeviceVector _ _) _ = error "Attempted to peek device vector!"

-- | Write an element to a vector
pokeVector :: Storable a => Vector a -> Int -> a -> IO ()
pokeVector (CVector _ p)    off = pokeElemOff p off
pokeVector (HostVector _ p) off = pokeElemOff (useHostPtr p) off
pokeVector (DeviceVector _ _) _ = error "Attempted to poke device vector!"

-- | Allocate a C vector using @malloc@ that is large enough for the
-- given number of elements.
-- Such allocated vector could be unsuitable for using with some AVX instructions
-- which require 32-bytes aligned data.
-- OTOH, we need grid/image data be padded on CPU to accelerate FFT considerably
--   and this could contradict the AVX alignment requirement.
-- Thus we have 2 options here: marshal the data to change their layout or switch
--   back to unaligned AVX instructions usage. The latter could be, perhaps,
--    the preferred way to deal with this.
allocCVector :: forall a. Storable a => Int -> IO (Vector a)
allocCVector n = fmap (CVector n) $ mallocBytes (n * s)
  where s = sizeOf (undefined :: a)

-- | Allocate a CUDA host vector in pinned memory with the given
-- number of elements.
allocHostVector :: Storable a => Int -> IO (Vector a)
allocHostVector n = fmap (HostVector n) $ mallocHostArray [] n

-- | Allocate a CUDA device array with the given number of elements
allocDeviceVector :: Storable a => Int -> IO (Vector a)
allocDeviceVector n = fmap (DeviceVector n) $ CUDA.mallocArray n

-- | Free data associated with the vector. It is generally required to
-- call this manually, or the data will remain valid!
--
-- This function will do nothing for vectors obtained using
-- @nullVector@ or @offsetVector@.
freeVector :: Vector a -> IO ()
freeVector (CVector 0 _)        = return ()
freeVector (CVector _ ptr)      = Foreign.Marshal.Alloc.free ptr
freeVector (HostVector 0 _)     = return ()
freeVector (HostVector _ ptr)   = freeHost ptr
freeVector (DeviceVector 0 _)   = return ()
freeVector (DeviceVector _ ptr) = CUDA.free ptr

-- | Convert the given vector into a C vector. The passed vector is
-- consumed.
toCVector :: Storable a => Vector a -> IO (Vector a)
toCVector v@CVector{}      = return v
toCVector (HostVector n p) = return $ CVector n (useHostPtr p)
                             -- Can use a host ptr as C ptr
toCVector v                = do v' <- dupCVector v; freeVector v; return v'

-- Slightly non-puristic signature (second Int parameter)
foreign import ccall unsafe cudaHostRegister :: Ptr a -> Int -> CUInt -> IO CInt
-- | Convert the given vector into a host vector. The passed vector is
-- consumed.
toHostVector :: forall a. Storable a => Vector a -> IO (Vector a)
toHostVector v@HostVector{} = return v
toHostVector v@(CVector n p) = do _ <- cudaHostRegister p n 0; return v
toHostVector v              = do v' <- dupHostVector v; freeVector v; return v'

-- | Convert the given vector into a device vector. The passed vector
-- is consumed.
toDeviceVector :: forall a. Storable a => Vector a -> IO (Vector a)
toDeviceVector v@DeviceVector{} = return v
toDeviceVector v                = do v' <- dupDeviceVector v; freeVector v; return v'

-- | Create a copy of the given vector as a C vector. Leaves the
-- original vector intact.
dupCVector :: forall a. Storable a => Vector a -> IO (Vector a)
dupCVector v = do
  let n = vectorSize v
      s = sizeOf (undefined :: a)
  v'@(CVector _ p') <- allocCVector n
  case v of
    CVector _ p      -> copyBytes p' p (s * n)
    HostVector _ p   -> copyBytes p' (useHostPtr p) (s * n)
    DeviceVector _ p -> peekArray n p p'
  return v'

-- | Create a copy of the given vector as a host vector. Leaves the
-- original vector intact.
dupHostVector :: forall a. Storable a => Vector a -> IO (Vector a)
dupHostVector (CVector n p) = do
  v'@(HostVector _ p') <- allocHostVector n
  let s = sizeOf (undefined :: a)
  copyBytes (useHostPtr p') p (s * n)
  return v'
dupHostVector (HostVector n p) = do
  v'@(HostVector _ p') <- allocHostVector n
  let s = sizeOf (undefined :: a)
  copyBytes (useHostPtr p') (useHostPtr p) (s * n)
  return v'
dupHostVector (DeviceVector n p) = do
  v'@(HostVector _ p') <- allocHostVector n
  peekArray n p (useHostPtr p')
  return v'

-- | Create a copy of the given vector as a device vector. Leaves the
-- original vector intact.
dupDeviceVector :: forall a. Storable a => Vector a -> IO (Vector a)
dupDeviceVector (CVector n p) = do
  v'@(DeviceVector _ p') <- allocDeviceVector n
  pokeArray n p p'
  return v'
dupDeviceVector (HostVector n p) = do
  v'@(DeviceVector _ p') <- allocDeviceVector n
  pokeArray n (useHostPtr p) p'
  return v'
dupDeviceVector (DeviceVector n p) = do
  v'@(DeviceVector _ p') <- allocDeviceVector n
  copyArray n p p'
  return v'
