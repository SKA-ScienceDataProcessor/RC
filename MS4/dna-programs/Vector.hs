{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Vector where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils

import Foreign.CUDA.Ptr as C
import Foreign.CUDA.Runtime as CUDA

import Foreign.Storable

-- | Vector type. Depending on the requirements of the kernel that
-- uses it, it might have a different underlying pointer type.
data Vector a
  = CVector !Int (Ptr a)
  | HostVector !Int (HostPtr a)
  | DeviceVector !Int (DevicePtr a)

-- | Allocate a C vector using @malloc@ that is large enough for the
-- given number of elements.
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
freeVector :: Vector a -> IO ()
freeVector (CVector _ ptr)      = Foreign.Marshal.Alloc.free ptr
freeVector (HostVector _ ptr)   = freeHost ptr
freeVector (DeviceVector _ ptr) = CUDA.free ptr

-- | Convert the given vector into a C vector. The passed vector is
-- consumed.
toCVector :: Storable a => Vector a -> IO (Vector a)
toCVector v@CVector{}      = return v
toCVector (HostVector n p) = return $ CVector n (useHostPtr p)
                             -- Can use a host ptr as C ptr
toCVector v@(DeviceVector n p) = do
  v'@(CVector _ p') <- allocCVector n
  peekArray n p p'
  freeVector v
  return v'

-- | Convert the given vector into a host vector. The passed vector is
-- consumed.
toHostVector :: forall a. Storable a => Vector a -> IO (Vector a)
toHostVector v@HostVector{} = return v
toHostVector v@(CVector n p) = do
  v'@(HostVector _ p') <- allocHostVector n
  let s = sizeOf (undefined :: a)
  copyBytes (useHostPtr p') p (s * n)
  freeVector v
  return v'
toHostVector v@(DeviceVector n p) = do
  v'@(HostVector _ p') <- allocHostVector n
  peekArray n p (useHostPtr p')
  freeVector v
  return v'

-- | Convert the given vector into a device vector. The passed vector
-- is consumed.
toDeviceVector :: forall a. Storable a => Vector a -> IO (Vector a)
toDeviceVector v@DeviceVector{} = return v
toDeviceVector v@(CVector n p) = do
  v'@(DeviceVector _ p') <- allocDeviceVector n
  pokeArray n p p'
  freeVector v
  return v'
toDeviceVector v@(HostVector n p) = do
  v'@(DeviceVector _ p') <- allocDeviceVector n
  pokeArray n (useHostPtr p) p'
  freeVector v
  return v'

