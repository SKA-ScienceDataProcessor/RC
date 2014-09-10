{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, BangPatterns #-}

module DNA.Channel.File (
          itemSize
        , requiredAlignmentInItems
        , readData, readDataMMap, roundUpDiv
        , chunkOffset, chunkSize
        , chunkOffsetWithAlignment, chunkSizeWithAlignment
        , FileVec(..), spawnFChan
        , module Data.Int
        ) where

import qualified Control.DeepSeq as CD

import Control.Distributed.Process
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe

import Control.Monad
import Control.Monad.Trans

import Data.Binary

import GHC.Generics (Generic)

import Data.Typeable

import Data.Int (Int64)

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Cfg

itemSize :: Int64
itemSize = 8

directReadAlignment :: Int64
directReadAlignment = 4096

requiredAlignmentInItems :: Int64
requiredAlignmentInItems = div directReadAlignment itemSize

-- divide up a file in chunkCount chunks
-- functions to read chunkSize * itemSize from a file at offset chunkSize * itemSize
roundUpDiv :: Int64 -> Int64 -> Int64
roundUpDiv a b = - div (-a) b

chunkOffset :: Int64 -> Int64 -> Int64 -> Int64
chunkOffset chunkCount itemCount chunkNo
    | chunkNo > chunkCount || chunkNo < 1 = -1
    | otherwise = itemSize * (chunkNo -1 ) * roundUpDiv itemCount chunkCount

chunkOffsetWithAlignment :: Int64 -> Int64 -> Int64 -> Int64 -> Int64
chunkOffsetWithAlignment alignment chunkCount itemCount chunkNo =
        alignment * chunkOffset chunkCount (div itemCount alignment) chunkNo

chunkSize :: Int64 -> Int64 -> Int64 -> Int64
chunkSize cC iC cN
    |  cN < 1 || cN > cC = 0
    |  cN > div iC (roundUpDiv iC cC) = iC - (cN -1) * (roundUpDiv iC cC)
    |  otherwise = roundUpDiv iC cC

chunkSizeWithAlignment :: Int64 -> Int64 -> Int64 -> Int64 -> Int64
chunkSizeWithAlignment alignment chunkCount itemCount chunkNo =
        alignment * chunkSize chunkCount (div itemCount alignment) chunkNo

-- read a buffer from a file into pinned memory
-- arguments: buffer ptr, size, offset, path
foreign import ccall unsafe "read_data"
    c_read_data :: Ptr CDouble -> CLong -> CLong -> CString -> IO ()

-- read a buffer from a file into mmapped memory.
-- arguments: size (num of elements of double type), offset, path
foreign import ccall unsafe "read_data_mmap"
    c_read_data_mmap :: CLong -> CLong -> CString -> IO (Ptr CDouble)

readData :: Int64 -> Int64 -> String -> IO (S.Vector Double)
readData n o p = do
        mv <- MS.new (fromIntegral n) :: IO (MS.IOVector Double)
        MS.unsafeWith mv $ \ptr ->
    -- Here I assume that CDouble and Double are same thing (it is)
    --     -- and blindly cast pointer
                withCString p (c_read_data (castPtr ptr) (fromIntegral n) (fromIntegral o))
        S.unsafeFreeze mv

readDataMMap :: Int64 -> Int64 -> String -> IO (S.Vector Double)
readDataMMap n o p = do
        ptr <- withCString p (c_read_data_mmap (fromIntegral n) (fromIntegral o))
        fptr <- newForeignPtr_ ptr
        return $ S.unsafeFromForeignPtr0 (castForeignPtr fptr) (fromIntegral n)


instance (S.Storable e, Binary e) => Binary (S.Vector e) where
        put vec = put (S.toList vec)
        get = get >>= (return . S.fromList)


data FileVec = FileVec ProcessId (S.Vector Double) deriving (Eq, Show, Typeable, Generic)

instance Binary FileVec where
        put (FileVec pid vec) = put pid >> put vec
        get = do { pid <- get; vec <- get; return (FileVec pid vec)}

instance CD.NFData FileVec where
        rnf (FileVec !procId !vec) = CD.rnf procId `seq` CD.rnf vec `seq` ()


spawnFChan :: String -> Int64 -> Int64 -> ProcessId -> Process()
spawnFChan path cO cS pid = do
        mypid <- getSelfPid
        iov <- timePeriod "reading file" $ liftIO $ readDataMMap cS cO path
        Unsafe.send pid (FileVec mypid iov)

