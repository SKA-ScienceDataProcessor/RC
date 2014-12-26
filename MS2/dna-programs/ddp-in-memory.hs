{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Data.Int
import qualified Data.Vector.Storable as S

import System.IO        ( openTempFile, hClose )
import System.Directory ( removeFile )

import DNA

import DDP



----------------------------------------------------------------
-- Distributed dot product
--
-- Note that actors which do not spawn actors on other nodes do not
-- receive CAD.
----------------------------------------------------------------

-- | Caclculate dot product of slice of vector
ddpProductSlice :: Actor (String,Int64) Double
ddpProductSlice = actor $ \(fname, size) -> duration "vector slice" $ do
    -- Calculate offsets
    nProc <- groupSize
    rnk   <- rank
    -- FIXME: Bad!
    let (off,n) = scatterShape (fromIntegral nProc) size !! rnk
    -- Start local processes
    resVA <- select Local (N 0)
    resVB <- select Local (N 0)
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
    -- Run generator & delay
    resG   <- select Local (N 0)
    shellG <- startActor resG $(mkStaticClosure 'ddpGenerateVector)
    sendParam (fname, size) shellG
    _ <- duration "generate" . await =<< delay Remote shellG

    -- Chunk & send out
    res   <- selectMany (Frac 1) (NNodes 1) [UseLocal]
    shell <- startGroup res $(mkStaticClosure 'ddpProductSlice)
    broadcastParam (fname,size) shell
    partials <- delayGroup shell
    x <- duration "collecting vectors" $ gather partials (+) 0
    return x

main :: IO ()
main = do
  (fname, h) <- openTempFile "." "temp.dat"
  dnaRun (DDP.__remoteTable . Main.__remoteTable) $ do
    b <- eval ddpDotProduct (fname,20000000)
    liftIO $ putStrLn $ "RESULT: " ++ show b
  hClose h
  removeFile fname
