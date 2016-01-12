{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Flow.Vector
  ( Vector(..)
  , vectorSize, vectorByteSize
  , nullVector
  , castVector
  , offsetVector
  , peekVector, pokeVector
  , makeVector, unmakeVector
  , copyVector
  -- * Memory Management
  , allocCVector
#ifdef USE_CUDA
  , allocHostVector, allocDeviceVector
#endif
  , freeVector
  -- * Conversion
  , toCVector, dupCVector
#ifdef USE_CUDA
  , toHostVector, dupHostVector
  , toDeviceVector, dupDeviceVector
#endif
  , unsafeToByteString, unsafeToByteString'
  , dumpVector, dumpVector'
  , readCVector
  , putVector, getVector
  ) where

import Control.Monad (when, forM_)
import Data.Binary   (Binary(..))
import Data.Binary.Put
import Data.Binary.Get
import Data.Int      (Int8)
import Data.Typeable (Typeable)
import Data.ByteString (ByteString, hPut)
import Data.ByteString.Unsafe (unsafePackAddressLen, unsafeUseAsCString)
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (advancePtr)
import Foreign.Marshal.Utils

#ifdef USE_CUDA
import Foreign.CUDA.Ptr as C
import Foreign.CUDA.Runtime as CUDA hiding (get)
import qualified Data.ByteString.Internal as BSI
#endif

import Foreign.Storable

import GHC.Exts     (Ptr(..))
import GHC.Generics (Generic)
import qualified GHC.IO.FD as FD
import qualified GHC.IO.Device as Dev

import System.IO
import System.IO.Unsafe

-- | The alignment that we are going to use for all vectors
vectorAlign :: CUInt
vectorAlign = 32

-- | Vector type. Depending on the requirements of the kernel that
-- uses it, it might have a different underlying pointer type.
data Vector a
  = CVector !Int (Ptr a)
#ifdef USE_CUDA
  | HostVector !Int (HostPtr a)
  | DeviceVector !Int (DevicePtr a)
#endif
  deriving (Show, Typeable, Generic)
-- FIXME: No sane binary  instance but we need to pass this data type around
instance Binary (Vector a) where
    get = error "getting vectors is undefined!"
    put = error "putting vectors is undefined!"

-- | Returns the number of elements a vector has. Note that this will
-- return @0@ for the result of "offsetVector".
vectorSize :: forall a. Storable a => Vector a -> Int
vectorSize v = (vectorByteSize v) `div` sizeOf (undefined :: a)

-- | Returns the size of the vector in bytes. Note that this will
-- return @0@ for the result of "offsetVector".
vectorByteSize :: Vector a -> Int
vectorByteSize (CVector n _) = n
#ifdef USE_CUDA
vectorByteSize (HostVector n _) = n
vectorByteSize (DeviceVector n _) = n
#endif

-- | A vector carrying no data, pointing nowhere
nullVector :: Vector a
nullVector = CVector 0 nullPtr

-- | Cast a vector to a different element type. Moderately evil.
castVector :: Vector a -> Vector b
castVector (CVector n p) = CVector n $ castPtr p
#ifdef USE_CUDA
castVector (HostVector n p) = HostVector n $ HostPtr $ castPtr $ useHostPtr p
castVector (DeviceVector n p) = DeviceVector n $ DevicePtr $ castPtr $ useDevicePtr p
#endif

-- | Make an at-offset vector. Note that this vector will only remain
-- valid as long as the original vector data isn't free.
offsetVector :: Storable a => Vector a -> Int -> Vector a
offsetVector (CVector _ p) off = CVector 0 $ p `advancePtr` off
#ifdef USE_CUDA
offsetVector (HostVector _ p) off = HostVector 0 $ p `advanceHostPtr` off
offsetVector (DeviceVector _ p) off = DeviceVector 0 $ p `advanceDevPtr` off
#endif

-- | Read an element from the vector
peekVector :: Storable a => Vector a -> Int -> IO a
peekVector (CVector _ p)    off = peekElemOff p off
#ifdef USE_CUDA
peekVector (HostVector _ p) off = peekElemOff (useHostPtr p) off
peekVector (DeviceVector _ _) _ = error "Attempted to peek device vector!"
#endif

-- | Write an element to a vector
pokeVector :: Storable a => Vector a -> Int -> a -> IO ()
pokeVector (CVector _ p)    off = pokeElemOff p off
#ifdef USE_CUDA
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
unmakeVector :: Storable a => Vector a -> Int -> Int -> IO [a]
#ifdef USE_CUDA
unmakeVector (DeviceVector _ p) off len = do
  v' <- dupCVector (DeviceVector len (p `advanceDevPtr` off))
  r <- unmakeVector v' 0 len
  freeVector v'
  return r
#endif
unmakeVector v off len = do
  (CVector _ p) <- toCVector v
  mapM (peekElemOff p) [off..off+len-1]

-- | Copies a portion of one vector into another vector
copyVector :: forall a. Storable a
           => Vector a -- ^ Output vector
           -> Int      -- ^ Output offset
           -> Vector a -- ^ Input vector
           -> Int      -- ^ Input offset
           -> Int      -- ^ Input length
           -> IO ()
copyVector (CVector _ outp) outoff (CVector _ inp) inoff inl
  = copyBytes (outp `advancePtr` outoff) (inp `advancePtr` inoff) (inl * sizeOf (undefined :: a))
#ifdef USE_CUDA
copyVector _ _ _ _ _
  = fail "copyVector only supported to C vectors so far - TODO!"
#endif

-- | Allocate a C vector using @malloc@ that is large enough for the
-- given number of elements. The returned vector will be aligned
-- according to "vectorAlign".
allocCVector :: forall a. Storable a => Int -> IO (Vector a)
#ifdef mingw32_HOST_OS
-- On Windows we can use _aligned_malloc directly
allocCVector n = fmap (CVector vs) $ c_aligned_malloc (fromIntegral vs) vectorAlign
  where vs = n * sizeOf (undefined :: a)
foreign import ccall unsafe "_aligned_malloc"
    c_aligned_malloc :: CUInt -> CUInt -> IO (Ptr a)
#else
-- The POSIX version is slightly less nice because just "memalign" is
-- apparently obsolete.
allocCVector n = alloca $ \pp -> do
  let vs = n * sizeOf (undefined :: a)
  ret <- c_posix_memalign pp vectorAlign (fromIntegral vs)
  when (ret /= 0) $
    ioError $ errnoToIOError "allocCVector" (Errno ret) Nothing Nothing
  p <- peek pp
  return $ CVector vs p
foreign import ccall unsafe "posix_memalign"
    c_posix_memalign :: Ptr (Ptr a) -> CUInt -> CUInt -> IO CInt
#endif

#ifdef USE_CUDA
-- | Allocate a CUDA host vector in pinned memory with the given
-- number of elements.
allocHostVector :: forall a. Storable a => Int -> IO (Vector a)
allocHostVector n = fmap (HostVector (n * sizeOf (undefined :: a))) $ mallocHostArray [] n

-- | Allocate a CUDA device array with the given number of elements
allocDeviceVector :: forall a. Storable a => Int -> IO (Vector a)
allocDeviceVector n = fmap (DeviceVector (n * sizeOf (undefined :: a))) $ CUDA.mallocArray n
#endif

-- | Free data associated with the vector. It is generally required to
-- call this manually, or the data will remain valid!
--

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "_aligned_free" c_aligned_free :: Ptr a -> IO ()
#endif

-- This function will do nothing for vectors obtained using
-- @nullVector@ or @offsetVector@.
freeVector :: Vector a -> IO ()
freeVector (CVector 0 _)   = return ()
freeVector (CVector _ ptr) =
#ifndef mingw32_HOST_OS
   Foreign.Marshal.Alloc.free ptr
#else 
   c_aligned_free ptr
#endif
#ifdef USE_CUDA
freeVector (HostVector 0 _)     = return ()
freeVector (HostVector _ ptr)   = freeHost ptr
freeVector (DeviceVector 0 _)   = return ()
freeVector (DeviceVector _ ptr) = CUDA.free ptr
#endif

-- | Convert the given vector into a C vector. The passed vector is
-- consumed.
toCVector :: Storable a => Vector a -> IO (Vector a)
toCVector v@CVector{}      = return v
#ifdef USE_CUDA
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
#endif

-- | Create a copy of the given vector as a C vector. Leaves the
-- original vector intact.
dupCVector :: forall a. Storable a => Vector a -> IO (Vector a)
dupCVector v = do
  v'@(CVector _ p') <- allocCVector (vectorSize v)
  case v of
    CVector _ p      -> copyBytes p' p (vectorByteSize v)
#ifdef USE_CUDA
    HostVector _ p   -> copyBytes p' (useHostPtr p) (vectorByteSize v)
    DeviceVector _ p -> peekArray (vectorSize v) p p'
#endif
  return v'

#ifdef USE_CUDA
-- | Create a copy of the given vector as a host vector. Leaves the
-- original vector intact.
dupHostVector :: Storable a => Vector a -> IO (Vector a)
dupHostVector v@(CVector _ p) = do
  v'@(HostVector _ p') <- allocHostVector (vectorSize v)
  copyBytes (useHostPtr p') p (vectorByteSize v)
  return v'
dupHostVector v@(HostVector _ p) = do
  v'@(HostVector _ p') <- allocHostVector (vectorSize v)
  copyBytes (useHostPtr p') (useHostPtr p) (vectorByteSize v)
  return v'
dupHostVector v@(DeviceVector _ p) = do
  v'@(HostVector _ p') <- allocHostVector (vectorSize v)
  peekArray (vectorSize v) p (useHostPtr p')
  return v'

-- | Create a copy of the given vector as a device vector. Leaves the
-- original vector intact.
dupDeviceVector :: Storable a => Vector a -> IO (Vector a)
dupDeviceVector v@(CVector _ p) = do
  v'@(DeviceVector _ p') <- allocDeviceVector (vectorSize v)
  pokeArray (vectorSize v) p p'
  return v'
dupDeviceVector v@(HostVector _ p) = do
  v'@(DeviceVector _ p') <- allocDeviceVector (vectorSize v)
  pokeArray (vectorSize v) (useHostPtr p) p'
  return v'
dupDeviceVector v@(DeviceVector _ p) = do
  v'@(DeviceVector _ p') <- allocDeviceVector (vectorSize v)
  copyArray (vectorSize v) p p'
  return v'
#endif

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
#ifdef USE_CUDA
unsafeToByteString' (HostVector _ (HostPtr p)) off size =
  case p `advancePtr` off of
    Ptr addr -> unsafePackAddressLen (size * sizeOf (undefined :: a)) addr
unsafeToByteString' (DeviceVector _ p) off size =
  BSI.create (size * sizeOf (undefined :: a)) $ \p' ->
    CUDA.sync >> peekArray size (p `advanceDevPtr` off) (castPtr p')
#endif

-- | Write vector to a file (raw)
dumpVector :: Storable a => Vector a -> FilePath ->  IO ()
dumpVector v file =
  withBinaryFile file WriteMode $ \h ->
    hPut h =<< unsafeToByteString v

-- | Write vector to a file (raw)
dumpVector' :: Storable a => Vector a -> Int -> Int -> FilePath ->  IO ()
dumpVector' v off size file = do
  withBinaryFile file WriteMode $ \h ->
    hPut h =<< unsafeToByteString' v off size

-- | Read vector from a file (raw)
readCVector :: Storable a => FilePath -> Int -> IO (Vector a)
readCVector file n = do
  v@(CVector _ p) <- allocCVector n
  (fd,_) <- FD.openFile file ReadMode False
  bytes <- Dev.read fd (castPtr p) (vectorByteSize v)
  when (bytes /= 0 && bytes /= vectorByteSize v) $
    fail $ "readCVector: Expected to read " ++ show (vectorByteSize v) ++
           " bytes, but only received " ++ show bytes ++ "!"
  Dev.close fd
  return v

putVector :: Vector a -> Put
putVector vec = do
  let vec' = castVector vec :: Vector Int8
      bs = unsafePerformIO $ unsafeToByteString vec'
  -- Apparently this already gets emitted by BS put?
  -- put (vectorByteSize vec)
  put bs

getVector :: Get (Vector a)
getVector = do
  size <- get :: Get Int
  bs <- getByteString size
  return $ unsafePerformIO $ do
    vec@(CVector _ vp) <- allocCVector size :: IO (Vector Int8)
    unsafeUseAsCString bs $ \p -> copyBytes vp (castPtr p) size
    return (castVector vec)
