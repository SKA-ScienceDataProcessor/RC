{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Data.Int
import qualified Data.Vector.Storable as S

import DNA

import DDP
import DDP_Slice


----------------------------------------------------------------
-- Distributed dot product
--
-- Note that actors which do not spawn actors on other nodes do not
-- receive CAD.
----------------------------------------------------------------

-- | Actor for calculating dot product
ddpDotProduct :: Actor (String,Slice) Double
ddpDotProduct = actor $ \(fname, size) -> do
    -- Chunk local product
    rnk   <- rank
    gSize <- groupSize
    let slice = scatterSlice gSize size !! rnk
    -- Chunk & send out
    res   <- selectMany (Frac 1) (NNodes 1) [UseLocal]
    shell <- startGroup res Failout $(mkStaticClosure 'ddpProductSlice)
    sendParam (fname,slice) $ broadcast shell
    partials <- delayGroup shell
    x <- duration "collecting vectors" $ gather partials (+) 0
    return x

remotable [ 'ddpDotProduct
          ]

ddpDotProductMaster :: Actor (String,Slice) Double
ddpDotProductMaster = actor $ \(fname,size) -> do
    res   <- selectMany (Frac 1) (NWorkers 3) [UseLocal]
    shell <- startGroup res Normal $(mkStaticClosure 'ddpDotProduct)
    sendParam (fname,size) $ broadcast shell
    partials <- delayGroup shell
    x <- duration "collection partials" $ gather partials (+) 0
    return x


main :: IO ()
main = do
    dnaRun rtable $ do
        b <- eval ddpDotProductMaster ("file.dat", Slice 0 20000000)
        liftIO $ putStrLn $ "RESULT: " ++ show b
  where
    rtable = DDP.__remoteTable
           . DDP_Slice.__remoteTable  
           . Main.__remoteTable
