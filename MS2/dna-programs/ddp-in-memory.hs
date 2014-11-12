{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Control.Applicative
import Control.Monad
import Data.Int
import qualified Data.Vector.Storable as S

import DNA.Channel.File (readDataMMap)
import DNA


-- | Split vector into set of slices.
scatterShape :: Int64 -> Int64 -> [(Int64,Int64)]
scatterShape n size
  = zipWith (,) chunkOffs chunkSizes
  where
    (chunk,rest) = size `divMod` n
    extra        = replicate (fromIntegral rest) 1 ++ repeat 0
    chunkSizes   = zipWith (+) (replicate (fromIntegral n) chunk) extra
    chunkOffs    = scanl (+) 0 chunkSizes



----------------------------------------------------------------
-- Distributed dot product
--
-- Note that actors which do not spawn actors on other nodes do not
-- receive CAD.
----------------------------------------------------------------

-- | Compute vector and send it back to master using unsafe send.
ddpComputeVector :: Actor (Int64,Int64) (S.Vector Double)
ddpComputeVector = actor $ \(off,n) -> do
    return $ S.generate (fromIntegral n)
               (\i -> fromIntegral (fromIntegral i + fromIntegral off))

-- | Read vector slice from the data file.
ddpReadVector :: Actor (String,(Int64,Int64)) (S.Vector Double)
ddpReadVector = actor $ \(fname, (off,n)) -> do
    liftIO $ readDataMMap n off fname "FIXME"


-- | Caclculate dot product of slice of vector
ddpProductSlice :: Actor (String,(Int64,Int64)) Double
ddpProductSlice = actor $ \(fname, slice) -> do
    futVA <- forkLocal NoNodes ddpComputeVector slice
    futVB <- forkLocal NoNodes ddpReadVector (fname :: String, slice :: (Int64,Int64))
    va <- await futVA
    vb <- await futVB
    return $ (S.sum $ S.zipWith (*) va vb :: Double)


remotable [ 'ddpComputeVector
          , 'ddpProductSlice
          ]


-- | Actor for calculating dot product
ddpDotProduct :: Actor (String,Int64) Double
ddpDotProduct = actor $ \(fname,size) -> do
    partials <- forkGroup ReqGroup $(mkStaticClosure 'ddpProductSlice)
                  ((,) <$> same fname <*> scatter (\n -> scatterShape (fromIntegral n)) size)
    gather partials (+) 0



main :: IO ()
main = dnaRun __remoteTable $ do
    b <- eval ddpDotProduct ("file.dat",1000000)
    liftIO $ print b
