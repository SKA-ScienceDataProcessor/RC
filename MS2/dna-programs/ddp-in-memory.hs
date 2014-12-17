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

import           DDP hiding (__remoteTable)
import qualified DDP 


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

-- | Caclculate dot product of slice of vector
ddpProductSlice :: Actor (String,Int64) Double
ddpProductSlice = actor $ \(fname, size) -> do
    -- Calculate offsets
    nProc <- groupSize
    rnk   <- rank
    -- FIXME: Bad!
    let (off,n) = scatterShape (fromIntegral nProc) size !! rnk
    -- Start local processes
    resVA <- select Local 0
    resVB <- select Local 0
    shellVA <- startActor resVA $(mkStaticClosure 'ddpComputeVector)
    shellVB <- startActor resVB $(mkStaticClosure 'ddpReadVector   )
    -- Connect actors
    sendParam (off,n)          shellVA 
    sendParam (fname, (off,n)) shellVB
    --
    futVA <- delay Local shellVA
    futVB <- delay Local shellVB
    --
    va <- duration "receive compute" $ await futVA
    vb <- duration "receive read"    $ await futVB
    --
    duration "compute sum" $
      return $ (S.sum $ S.zipWith (*) va vb :: Double)

remotable [ 'ddpProductSlice
          ]


-- | Actor for calculating dot product
ddpDotProduct :: Actor (String,Int64) Double
ddpDotProduct = actor $ \(fname,size) -> do
    logMessage "YAY"
    res   <- selectMany 4
    shell <- startGroup res $(mkStaticClosure 'ddpProductSlice)
    broadcastParam (fname,size) shell
    partials <- delayGroup shell
    x <- gather partials (+) 0
    return x

main :: IO ()
main = dnaRun (DDP.__remoteTable . __remoteTable) $ do
    b <- eval ddpDotProduct ("file.dat",20000000)
    liftIO $ putStrLn $ "RESULT: " ++ show b
