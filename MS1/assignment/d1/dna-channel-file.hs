module DNA.Channel.File (itemSize, readData, roundUpDiv, chunkOffset, chunkSize) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS

itemSize :: Int
itemSize = 8
-- divide up a file in chunkCount chunks
-- functions to read chunkSize * itemSize from a file at offset chunkSize * itemSize
roundUpDiv :: Int -> Int -> Int
roundUpDiv a b = - div (-a) b

chunkOffset :: Int -> Int -> Int -> Int
chunkOffset chunkCount itemCount chunkNo
    | chunkNo > chunkCount || chunkNo < 1 = -1
    | otherwise = (chunkNo -1 ) * roundUpDiv itemCount chunkCount


chunkSize :: Int -> Int -> Int -> Int
chunkSize cC iC cN
    |  cN < 1 || cN > cC = 0
    |  cN > div iC (roundUpDiv iC cC) = iC - (cN -1) * (roundUpDiv iC cC)
    |  otherwise = roundUpDiv iC cC

-- read a buffer from a file into pinned memory
-- arguments: buffer ptr, size, offset, path
foreign import ccall unsafe "buffer-io.h read_data"
    c_read_data :: Ptr CDouble -> CInt -> CInt -> CString -> IO ()

readData :: Int -> Int -> String -> IO (S.Vector Double)
readData n o p = do
	mv <- MS.new n :: IO (MS.IOVector Double)
  	MS.unsafeWith mv $ \ptr ->
    -- Here I assume that CDouble and Double are same thing (it is)
    --     -- and blindly cast pointer
        	withCString p (c_read_data (castPtr ptr) (fromIntegral n) (fromIntegral o))
        S.unsafeFreeze mv

