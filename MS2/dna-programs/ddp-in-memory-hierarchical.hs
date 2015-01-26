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
ddpDotProduct :: Actor Slice Double
ddpDotProduct = actor $ \size -> do
    -- Chunk local product
    rnk   <- rank
    gSize <- groupSize
    let slice = scatterSlice gSize size !! rnk
    -- Chunk & send out
    res   <- selectMany (Frac 1) (NNodes 1) [UseLocal]
    shell <- startGroup res Failout $(mkStaticClosure 'ddpProductSlice)
    sendParam slice $ broadcast shell
    partials <- delayGroup shell
    x <- duration "collecting vectors" $ gather partials (+) 0
    return x

remotable [ 'ddpDotProduct
          ]

ddpDotProductMaster :: Actor Slice Double
ddpDotProductMaster = actor $ \size -> do
    res   <- selectMany (Frac 1) (NWorkers 3) [UseLocal]
    shell <- startGroup res Normal $(mkStaticClosure 'ddpDotProduct)
    sendParam size $ broadcast shell
    partials <- delayGroup shell
    x <- duration "collection partials" $ gather partials (+) 0
    return x


main :: IO ()
main = do
    dnaRun rtable $ do
        b <- eval ddpDotProductMaster (Slice 0 (20*1000*1000))
        liftIO $ putStrLn $ "RESULT: " ++ show b
  where
    rtable = DDP.__remoteTable
           . DDP_Slice.__remoteTable  
           . Main.__remoteTable
