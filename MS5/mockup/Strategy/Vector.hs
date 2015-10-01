{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Strategy.Vector
  ( Vector(..)
  , vectorSize, vectorByteSize
  , nullVector
  , castVector
  , offsetVector
  , peekVector, pokeVector
  , makeVector, unmakeVector
  -- * Memory Management
  , allocCVector
  , freeVector
  ) where

import Control.Monad (when, forM_)
import Data.Binary   (Binary(..))
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (advancePtr)

import Foreign.Storable

import GHC.Generics (Generic)

-- | The alignment that we are going to use for all vectors
vectorAlign :: CUInt
vectorAlign = 32

-- | Vector type. Depending on the requirements of the kernel that
-- uses it, it might have a different underlying pointer type.
data Vector a
  = CVector !Int (Ptr a)
  deriving (Show, Typeable, Generic)
-- FIXME: No sane binary  instance but we need to pass this data type around
instance Binary (Vector a) where
    get = error "getting vectors is undefined!"
    put = error "putting vectors is undefined!"

-- | Returns the number of elements a vector has. Note that this will
-- return @0@ for the result of "offsetVector".
vectorSize :: Vector a -> Int
vectorSize (CVector n _) = n

-- | Returns the size of the vector in bytes. Note that this will
-- return @0@ for the result of "offsetVector".
vectorByteSize :: forall a. Storable a => Vector a -> Int
vectorByteSize v = (vectorSize v) * sizeOf (undefined :: a)

-- | A vector carrying no data, pointing nowhere
nullVector :: Vector a
nullVector = CVector 0 nullPtr

-- | Cast a vector to a different element type. Moderately evil.
castVector :: Vector a -> Vector b
castVector (CVector n p) = CVector n $ castPtr p

-- | Make an at-offset vector. Note that this vector will only remain
-- valid as long as the original vector data isn't free.
offsetVector :: Storable a => Vector a -> Int -> Vector a
offsetVector (CVector _ p) off = CVector 0 $ p `advancePtr` off

-- | Read an element from the vector
peekVector :: Storable a => Vector a -> Int -> IO a
peekVector (CVector _ p)    off = peekElemOff p off

-- | Write an element to a vector
pokeVector :: Storable a => Vector a -> Int -> a -> IO ()
pokeVector (CVector _ p)    off = pokeElemOff p off
#ifdef CUDA
pokeVector (HostVector _ p) off = pokeElemOff (useHostPtr p) off
pokeVector (DeviceVector _ _) _ = error "Attempted to poke device vector!"
#endif

-- | Make a vector populated with values from the given list
makeVector :: Storable a => (Int -> IO (Vector a)) -> [a] -> IO (Vector a)
makeVector alloc vs = do
  let len = length vs
  vec <- alloc len
  forM_ (zip [0..len-1] vs) $ uncurry (pokeVector vec)
  return vec

-- | Show vector contents
unmakeVector :: (Show a, Storable a) => Vector a -> Int -> Int -> IO [a]
unmakeVector v off len = do
  (CVector _ p) <- toCVector v
  mapM (peekElemOff p) [off..off+len-1]

-- | Allocate a C vector using @malloc@ that is large enough for the
-- given number of elements. The returned vector will be aligned
-- according to "vectorAlign".
allocCVector :: forall a. Storable a => Int -> IO (Vector a)
#ifdef _WIN32
-- On Windows we can use _aligned_malloc directly
allocCVector n = fmap (CVector n) $ c_aligned_malloc vectorAlign vs
  where vs = fromIntegral $ n * sizeOf (undefined :: a)
foreign import ccall unsafe "_aligned_malloc"
    c_aligned_malloc :: CUInt -> CUInt -> IO (Ptr a)
#else
-- The POSIX version is slightly less nice because just "memalign" is
-- apparently obsolete.
allocCVector n = alloca $ \pp -> do
  let vs = fromIntegral $ n * sizeOf (undefined :: a)
  ret <- c_posix_memalign pp vectorAlign vs
  when (ret /= 0) $
    ioError $ errnoToIOError "allocCVector" (Errno ret) Nothing Nothing
  p <- peek pp
  return $ CVector n p
foreign import ccall unsafe "posix_memalign"
    c_posix_memalign :: Ptr (Ptr a) -> CUInt -> CUInt -> IO CInt
#endif

-- | Free data associated with the vector. It is generally required to
-- call this manually, or the data will remain valid!
--
-- This function will do nothing for vectors obtained using
-- @nullVector@ or @offsetVector@.
freeVector :: Vector a -> IO ()
freeVector (CVector 0 _)        = return ()
freeVector (CVector _ ptr)      = Foreign.Marshal.Alloc.free ptr

-- | Convert the given vector into a C vector. The passed vector is
-- consumed.
toCVector :: Storable a => Vector a -> IO (Vector a)
toCVector v@CVector{}      = return v
