{-# LANGUAGE ForeignFunctionInterface #-}
-- | Channel for reading data from file
module DNA.Channel where

import Control.Distributed.Process

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS

import Foreign
import Foreign.C.Types
import Foreign.C.String

import DNA.CH


-- read a buffer from a file into pinned memory
-- arguments: buffer ptr, size, offset, path
foreign import ccall "read_data"
  c_read_data :: Ptr CDouble -> CInt -> CInt -> CString -> IO ()

-- read a buffer from a file into mmapped memory.
-- arguments: size (num of elements of double type), offset, path
foreign import ccall "read_data_mmap"
  c_read_data_mmap :: CInt -> CInt -> CString -> IO (Ptr CDouble)


readShapeWith :: (Int -> Int -> String -> IO (S.Vector Double))
              -> String -> Shape -> Process (S.Vector Double)
readShapeWith f nm (Shape n) = liftIO $ f n 0 nm

readSliceWith :: (Int -> Int -> String -> IO (S.Vector Double))
              -> String -> Slice -> Process (S.Vector Double)
readSliceWith f nm (Slice off n) = liftIO $ f n off nm


readData :: Int -> Int -> String -> IO (S.Vector Double)
readData n o p = do
  mv <- MS.new n :: IO (MS.IOVector Double)
  MS.unsafeWith mv $ \ptr ->
    withCString p (c_read_data (castPtr ptr) (fromIntegral n) (fromIntegral o))
  S.unsafeFreeze mv

readDataMMap :: Int -> Int -> String -> IO (S.Vector Double)
readDataMMap n o p = do
  ptr <- withCString p (c_read_data_mmap (fromIntegral n) (fromIntegral o))
  fptr <- newForeignPtr_ ptr
  return $ S.unsafeFromForeignPtr0 (castForeignPtr fptr) n


