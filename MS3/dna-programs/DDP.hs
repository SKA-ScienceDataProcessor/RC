{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module DDP where

import Control.Monad
import Data.Int
import qualified Data.Vector.Storable as S

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)

import System.IO        ( openBinaryTempFile, hClose )
import System.Directory ( doesDirectoryExist)

import DNA.Channel.File (readData {-,readDataMMap-})
import DNA

----------------------------------------------------------------
-- Workers for distributed dot product
----------------------------------------------------------------

-- | Single slice of 1D dimensional vector.
--
-- > Slice offset size
data Slice = Slice Int64 Int64
             deriving (Show,Typeable,Generic)
instance Binary Slice


-- | Split vector into set of slices.
scatterSlice :: Int -> Slice -> [Slice]
scatterSlice n (Slice off0 size)
  = zipWith (\o s -> Slice (off0 + o) s) chunkOffs chunkSizes
  where
    (chunk,rest) = size `divMod` fromIntegral n
    extra        = replicate (fromIntegral rest) 1 ++ repeat 0
    chunkSizes   = zipWith (+) (replicate (fromIntegral n) chunk) extra
    chunkOffs    = scanl (+) 0 chunkSizes


-- | Compute vector and send it back to master using unsafe send.
--
--   > Input:  part of vector to generate
--   > Output: data
ddpComputeVector :: Actor Slice (S.Vector Double)
ddpComputeVector = actor $ \(Slice off n) ->
  profile "compute vector" [HaskellHint (fromIntegral $ n * 8)] $ do
    return $ S.generate (fromIntegral n)
               (\i -> fromIntegral (fromIntegral i + off))

-- | Read vector slice from the data file.
--
--    * Input:  (file name and slice to read from vector)
--    * Output: data
ddpReadVector :: Actor (String, Slice) (S.Vector Double)
ddpReadVector = actor $ \(fname, Slice off n) ->
  profile "read vector" [IOHint (fromIntegral $ n * 8) 0] $ do
    -- FIXME: mmaping of vector is not implemented
    liftIO $ readData n off fname
    -- liftIO $ readDataMMap n off fname "FIXME"

-- | Fill the file with an example vector of the given size
--
--    * Input:  size of vector to generate
--    * Output: name of generated file
ddpGenerateVector :: Actor Int64 String
ddpGenerateVector = actor $ \n ->
  profile "generate vector" [IOHint 0 (fromIntegral $ n * 8),
                             HaskellHint (fromIntegral $ n * 8)] $
    liftIO $ do
      -- On Cambridge cluster we we write to the /ramdisk directory
      -- Otherwise we write to local directory
      let ramdisk = "/ramdisks"
      isCambridge <- doesDirectoryExist ramdisk
      (fname, h)  <- openBinaryTempFile
                       (if isCambridge then ramdisk else ".")
                       "temp.dat"
      BS.hPut h $ runPut $ do
        replicateM_ (fromIntegral n) $ putFloat64le 0.1
      hClose h
      return fname

ddpCollector :: CollectActor Double Double
ddpCollector = collectActor
    (\s a -> return $! s + a)
    (return 0)
     return


remotable [ 'ddpComputeVector
          , 'ddpReadVector
          , 'ddpGenerateVector
          , 'ddpCollector
          ]
