{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, BangPatterns #-}

module DNA.Channel.File (itemSize, readData, readDataMMap, roundUpDiv, chunkOffset, chunkSize, FileVec(..), spawnFChan) where

import qualified Control.DeepSeq as CD

import Control.Distributed.Process
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe

import Control.Monad
import Control.Monad.Trans

import Data.Binary

import GHC.Generics (Generic)

import Data.Typeable

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Cfg

itemSize :: Int
itemSize = 8

-- divide up a file in chunkCount chunks
-- functions to read chunkSize * itemSize from a file at offset chunkSize * itemSize
roundUpDiv :: Int -> Int -> Int
roundUpDiv a b = - div (-a) b

chunkOffset :: Int -> Int -> Int -> Int
chunkOffset chunkCount itemCount chunkNo
    | chunkNo > chunkCount || chunkNo < 1 = -1
    | otherwise = itemSize * (chunkNo -1 ) * roundUpDiv itemCount chunkCount


chunkSize :: Int -> Int -> Int -> Int
chunkSize cC iC cN
    |  cN < 1 || cN > cC = 0
    |  cN > div iC (roundUpDiv iC cC) = iC - (cN -1) * (roundUpDiv iC cC)
    |  otherwise = roundUpDiv iC cC

-- read a buffer from a file into pinned memory
-- arguments: buffer ptr, size, offset, path
foreign import ccall unsafe "read_data"
    c_read_data :: Ptr CDouble -> CInt -> CInt -> CString -> IO ()

-- read a buffer from a file into mmapped memory.
-- arguments: size (num of elements of double type), offset, path
foreign import ccall unsafe "read_data_mmap"
    c_read_data_mmap :: CInt -> CInt -> CString -> IO (Ptr CDouble)

readData :: Int -> Int -> String -> IO (S.Vector Double)
readData n o p = do
        mv <- MS.new n :: IO (MS.IOVector Double)
        MS.unsafeWith mv $ \ptr ->
    -- Here I assume that CDouble and Double are same thing (it is)
    --     -- and blindly cast pointer
                withCString p (c_read_data (castPtr ptr) (fromIntegral n) (fromIntegral o))
        S.unsafeFreeze mv

readDataMMap :: Int -> Int -> String -> IO (S.Vector Double)
readDataMMap n o p = do
        ptr <- withCString p (c_read_data_mmap (fromIntegral n) (fromIntegral o))
        fptr <- newForeignPtr_ ptr
        return $ S.unsafeFromForeignPtr0 (castForeignPtr fptr) n


instance (S.Storable e, Binary e) => Binary (S.Vector e) where
        put vec = put (S.toList vec)
        get = get >>= (return . S.fromList)


data FileVec = FileVec ProcessId (S.Vector Double) deriving (Eq, Show, Typeable, Generic)

instance Binary FileVec where
        put (FileVec pid vec) = put pid >> put vec
        get = do { pid <- get; vec <- get; return (FileVec pid vec)}

instance CD.NFData FileVec where
        rnf (FileVec !procId !vec) = CD.rnf procId `seq` CD.rnf vec `seq` ()


spawnFChan :: String -> Int -> Int -> ProcessId -> Process()
spawnFChan path cO cS pid = do
        mypid <- getSelfPid
        iov <- timePeriod "reading file" $ liftIO $ readData cS cO path
        Unsafe.send pid (FileVec mypid iov)

