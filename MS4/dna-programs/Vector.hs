{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Vector
  ( Vector(..)
  , vectorSize, vectorByteSize
  , nullVector
  , castVector
  , offsetVector
  , peekVector, pokeVector
  , makeVector, unmakeVector
  -- * Memory Management
  , allocCVector, allocHostVector, allocDeviceVector
  , freeVector
  -- * Conversion
  , toCVector, toHostVector, toDeviceVector
  , dupCVector, dupHostVector, dupDeviceVector
  , unsafeToByteString, unsafeToByteString'
  , dumpVector, dumpVector'
  ) where

import Control.Monad (when, forM_)
import Data.Binary   (Binary(..))
import Data.Typeable (Typeable)
import Data.ByteString (ByteString, hPut)
import Data.ByteString.Unsafe (unsafePackAddressLen)
import qualified Data.ByteString.Internal as BSI
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (advancePtr)
import Foreign.Marshal.Utils

import Foreign.CUDA.Ptr as C
import Foreign.CUDA.Runtime as CUDA

import Foreign.Storable

import GHC.Exts     (Ptr(..))
import GHC.Generics (Generic)

import System.IO

-- | The alignment that we are going to use for all vectors
vectorAlign :: CUInt
vectorAlign = 32

-- | Vector type. Depending on the requirements of the kernel that
-- uses it, it might have a different underlying pointer type.
data Vector a
  = CVector !Int (Ptr a)
  | HostVector !Int (HostPtr a)
  | DeviceVector !Int (DevicePtr a)
  deriving (Show, Typeable, Generic)
-- FIXME: No sane binary  instance but we need to pass this data type around
instance Binary (Vector a) where
    get = error "getting vectors is undefined!"
    put = error "putting vectors is undefined!"

-- | Returns the number of elements a vector has. Note that this will
-- return @0@ for the result of "offsetVector".
vectorSize :: Vector a -> Int
vectorSize (CVector n _) = n
vectorSize (HostVector n _) = n
vectorSize (DeviceVector n _) = n

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

-- | Make a vector populated with values from the given list
makeVector :: Storable a => (Int -> IO (Vector a)) -> [a] -> IO (Vector a)
makeVector alloc vs = do
  let len = length vs
  vec <- alloc len
  forM_ (zip [0..len-1] vs) $ uncurry (pokeVector vec)
  return vec

-- | Show vector contents
unmakeVector :: (Show a, Storable a) => Vector a -> Int -> Int -> IO [a]
unmakeVector (DeviceVector _ p) off len = do
  v' <- dupCVector (DeviceVector len (p `advanceDevPtr` off))
  r <- unmakeVector v' 0 len
  freeVector v'
  return r
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
toHostVector v@HostVector{}  = return v
toHostVector v@(CVector n p) = do _ <- cudaHostRegister p (vectorByteSize v) 0
                                  return (HostVector n (HostPtr p))
toHostVector v               = do v' <- dupHostVector v; freeVector v; return v'

-- | Convert the given vector into a device vector. The passed vector
-- is consumed.
toDeviceVector :: forall a. Storable a => Vector a -> IO (Vector a)
toDeviceVector v@DeviceVector{} = return v
toDeviceVector v                = do v' <- dupDeviceVector v; freeVector v; return v'

-- | Create a copy of the given vector as a C vector. Leaves the
-- original vector intact.
dupCVector :: forall a. Storable a => Vector a -> IO (Vector a)
dupCVector v = do
  v'@(CVector _ p') <- allocCVector (vectorSize v)
  case v of
    CVector _ p      -> copyBytes p' p (vectorByteSize v)
    HostVector _ p   -> copyBytes p' (useHostPtr p) (vectorByteSize v)
    DeviceVector _ p -> peekArray (vectorSize v) p p'
  return v'

-- | Create a copy of the given vector as a host vector. Leaves the
-- original vector intact.
dupHostVector :: forall a. Storable a => Vector a -> IO (Vector a)
dupHostVector v@(CVector n p) = do
  v'@(HostVector _ p') <- allocHostVector n
  copyBytes (useHostPtr p') p (vectorByteSize v)
  return v'
dupHostVector v@(HostVector n p) = do
  v'@(HostVector _ p') <- allocHostVector n
  copyBytes (useHostPtr p') (useHostPtr p) (vectorByteSize v)
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

-- | Turn a vector into a bytestring referencing the same data. This
-- is unsafe insofar that changes to the vector might change the
-- bytestring, and freeing it might cause crashes.
unsafeToByteString :: Storable a => Vector a -> IO ByteString
unsafeToByteString v = unsafeToByteString' v 0 (vectorSize v)

-- | As @unsafeByteString@, but allows specifying the vector's offset
-- and size.
unsafeToByteString' :: forall a. Storable a => Vector a -> Int -> Int -> IO ByteString
unsafeToByteString' (CVector _ p) off size =
  case p `advancePtr` off of
    Ptr addr -> unsafePackAddressLen (size * sizeOf (undefined :: a)) addr
unsafeToByteString' (HostVector _ (HostPtr p)) off size =
  case p `advancePtr` off of
    Ptr addr -> unsafePackAddressLen (size * sizeOf (undefined :: a)) addr
unsafeToByteString' (DeviceVector _ p) off size =
  BSI.create (size * sizeOf (undefined :: a)) $ \p' ->
    CUDA.sync >> peekArray size (p `advanceDevPtr` off) (castPtr p')

-- | Write vector to a file (raw)
dumpVector :: Storable a => Vector a -> FilePath ->  IO ()
dumpVector v file =
  withFile file WriteMode $ \h ->
    hPut h =<< unsafeToByteString v

-- | Write vector to a file (raw)
dumpVector' :: Storable a => Vector a -> Int -> Int -> FilePath ->  IO ()
dumpVector' v off size file = do
  withFile file WriteMode $ \h ->
    hPut h =<< unsafeToByteString' v off size
