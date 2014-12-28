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

import DNA.Channel.File (readData,readDataMMap)
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
ddpComputeVector :: Actor Slice (S.Vector Double)
ddpComputeVector = actor $ \(Slice off n) -> duration "compute vector" $ do
    return $ S.generate (fromIntegral n)
               (\i -> fromIntegral (fromIntegral i + off))

-- | Read vector slice from the data file.
ddpReadVector :: Actor (String, Slice) (S.Vector Double)
ddpReadVector = actor $ \(fname, Slice off n) -> duration "read vector" $ do
    liftIO $ readData n off fname
    -- liftIO $ readDataMMap n off fname "FIXME"

-- | Fill the file with an example vector of the given size
ddpGenerateVector :: Actor (String,Int64) ()
ddpGenerateVector = actor $ \(fname, n) -> duration "generate vector" $ do
    liftIO $ BS.writeFile fname $ runPut $
      replicateM_ (fromIntegral n) (putFloat64le 0.1)

remotable [ 'ddpComputeVector
          , 'ddpReadVector
          , 'ddpGenerateVector
          ]
