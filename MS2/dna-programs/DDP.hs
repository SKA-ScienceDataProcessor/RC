{-# LANGUAGE TemplateHaskell #-}
module DDP where

import Control.Applicative
import Control.Monad
import Data.Int
import qualified Data.Vector.Storable as S

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put
import Data.Binary.IEEE754

import DNA.Channel.File (readDataMMap)
import DNA

----------------------------------------------------------------
-- Workers for distributed dot product
----------------------------------------------------------------

-- | Split vector into set of slices.
scatterShape :: Int64 -> Int64 -> [(Int64,Int64)]
scatterShape n size
  = zipWith (,) chunkOffs chunkSizes
  where
    (chunk,rest) = size `divMod` n
    extra        = replicate (fromIntegral rest) 1 ++ repeat 0
    chunkSizes   = zipWith (+) (replicate (fromIntegral n) chunk) extra
    chunkOffs    = scanl (+) 0 chunkSizes



-- | Compute vector and send it back to master using unsafe send.
ddpComputeVector :: Actor (Int64,Int64) (S.Vector Double)
ddpComputeVector = actor $ \(off,n) -> duration "compute vector" $ do
    return $ S.generate (fromIntegral n)
               (\i -> fromIntegral (fromIntegral i + off))

-- | Read vector slice from the data file.
ddpReadVector :: Actor (String,(Int64,Int64)) (S.Vector Double)
ddpReadVector = actor $ \(fname, (off,n)) -> duration "read vector" $ do
    liftIO $ readDataMMap n off fname "FIXME"

-- | Fill the file with an example vector of the given size
ddpGenerateVector :: Actor (String,Int64) ()
ddpGenerateVector = actor $ \(fname, n) -> duration "generate vector" $ do
    liftIO $ BS.writeFile fname $ runPut $
      replicateM_ (fromIntegral n) (putFloat64le 0.1)

remotable [ 'ddpComputeVector
          , 'ddpReadVector
          , 'ddpGenerateVector
          ]
