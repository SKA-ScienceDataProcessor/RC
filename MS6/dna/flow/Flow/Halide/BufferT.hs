{-# LANGUAGE CPP #-}
-- |
-- Working with buffer_t struct
--
-- FIXME: offsets in struct are hardcoded!
module Flow.Halide.BufferT where

import Control.Monad
import Data.Int
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable

#ifdef USE_CUDA
import Foreign.CUDA.Ptr (DevicePtr, nullDevPtr)
#endif

-- | Wrapper for buffer_t data type. It's managed by haskell GC
--
-- I don't remember how to use tools to generate accessors from
-- headers so I'll just hard code offsets.
newtype BufferT = BufferT (ForeignPtr ())

-- | Allocate new buffer_t and
newBufferT :: IO BufferT
newBufferT = do
  let size = 72
  fptr <- mallocForeignPtrBytes size
  -- Zero out memory
  withForeignPtr fptr $ \p ->
    forM_ [0 .. 72 `div` 8] $ \i ->
      pokeElemOff (castPtr p) i (0 :: Int64)
  return $ BufferT fptr

withBufferT :: BufferT -> (Ptr BufferT -> IO a) -> IO a
withBufferT (BufferT fptr) action =
  withForeignPtr fptr (action . castPtr)

setHostPtr :: BufferT -> Ptr () -> IO ()
setHostPtr (BufferT fptr) buf =
  withForeignPtr fptr $ \p ->
    pokeByteOff (castPtr p) 8 buf

setDirty :: Int -> BufferT -> Bool -> IO ()
setDirty off (BufferT fptr) d =
  withForeignPtr fptr $ \p ->
    poke (p `plusPtr` off) dbyte
  where
    dbyte :: Int8
    dbyte = fromIntegral (fromEnum d)

setHostDirty, setDeviceDirty :: BufferT -> Bool -> IO ()
setHostDirty = setDirty 68
setDeviceDirty = setDirty 69

setDirtyHostPtr :: BufferT -> Ptr () -> IO ()
setDirtyHostPtr buf p = setHostPtr buf p >> setHostDirty buf True

#ifdef USE_CUDA
-- foreign import ccall unsafe
halide_cuda_wrap_device_ptr :: Ptr () -> Ptr () -> DevicePtr a -> IO ()
-- Not enabled yet
halide_cuda_wrap_device_ptr _user_ctx _buffer_t _dev_ptr = return ()

setDevicePtr :: BufferT -> DevicePtr a -> IO ()
setDevicePtr (BufferT fptr) buf =
  withForeignPtr fptr $ \p ->
    halide_cuda_wrap_device_ptr nullPtr p buf

-- foreign import ccall unsafe
halide_cuda_detach_device_ptr :: Ptr () -> Ptr () -> IO (DevicePtr a)
-- Not enabled yet. Stub only
halide_cuda_detach_device_ptr _user_ctx _buffer_t = return nullDevPtr

detachDevicePtr :: BufferT -> IO (DevicePtr a)
detachDevicePtr (BufferT fptr) =
  withForeignPtr fptr $ halide_cuda_detach_device_ptr nullPtr
#endif

setBufferExtents :: BufferT -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
setBufferExtents (BufferT fptr) a b c d =
  withForeignPtr fptr $ \p ->
    pokeArray (castPtr $ p `plusPtr` 16) [a,b,c,d]

setBufferStride :: BufferT -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
setBufferStride (BufferT fptr) a b c d =
  withForeignPtr fptr $ \p ->
    pokeArray (castPtr $ p `plusPtr` 32) [a,b,c,d]

setBufferMin :: BufferT -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
setBufferMin (BufferT fptr) a b c d =
  withForeignPtr fptr $ \p ->
    pokeArray (castPtr $ p `plusPtr` 48) [a,b,c,d]

setElemSize :: BufferT -> Int -> IO ()
setElemSize (BufferT fptr) n =
  withForeignPtr fptr $ \p ->
    pokeByteOff (castPtr p) 64 (fromIntegral n :: Int32)

