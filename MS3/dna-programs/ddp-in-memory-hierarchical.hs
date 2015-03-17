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
    shell <- startGroup (Frac 1) (NNodes 1) $ do
        useLocal
        return $(mkStaticClosure 'ddpProductSlice)
    sendParam slice $ broadcast shell
    partials <- delayGroup shell
    x <- {- duration "collecting vectors" $ -} gather partials (+) 0
    return x

remotable [ 'ddpDotProduct
          ]

ddpDotProductMaster :: Actor Slice Double
ddpDotProductMaster = actor $ \size -> do
    shell <- startGroup (Frac 1) (NWorkers 3) $ do
        useLocal
        return $(mkStaticClosure 'ddpDotProduct)
    sendParam size $ broadcast shell
    partials <- delayGroup shell
    x <- {- duration "collection partials" $ -} gather partials (+) 0
    return x


main :: IO ()
main = dnaRun rtable $ do
    -- Vector size:
    --
    -- > 100e4 doubles per node = 800 MB per node
    -- > 20 nodes
    let n        = 2000*1000*1000
        expected = fromIntegral n*(fromIntegral n-1)/2 * 0.1
    -- Run it
    b <- eval ddpDotProduct (Slice 0 n)
    liftIO $ putStrLn $ concat
      [ "RESULT: ", show b
      , " EXPECTED: ", show expected
      , if b == expected then " -- ok" else " -- WRONG!"
      ]
  where
    rtable = DDP.__remoteTable
           . DDP_Slice.__remoteTable  
           . Main.__remoteTable
