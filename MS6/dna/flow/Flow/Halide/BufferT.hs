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
import Foreign.CUDA.Ptr (DevicePtr)
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

#ifdef USE_CUDA
setDevicePtr :: BufferT -> DevicePtr a -> IO ()
setDevicePtr (BufferT fptr) buf =
  withForeignPtr fptr $ \p ->
    pokeElemOff (castPtr p) 0 buf
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

