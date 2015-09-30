{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Data.Int

import DNA

import DDP
import DDP_Slice


----------------------------------------------------------------
-- Distributed dot product
----------------------------------------------------------------

-- | Actor for calculating dot product
ddpDotProduct :: Actor Int64 Double
ddpDotProduct = actor $ \size -> do
    -- Chunk & send out
    shell <- startGroup (Frac 1) (NNodes 1) $ do
        useLocal
        return $(mkStaticClosure 'ddpProductSlice)
    broadcast (Slice 0 size) shell
    -- Collect results
    partials <- delayGroup shell
    x <- duration "collecting vectors" $ gather partials (+) 0
    return x

main :: IO ()
main =  dnaRun rtable $ do
    -- Vector size:
    --
    -- > 100e4 doubles per node = 800 MB per node
    -- > 4 nodes
    let n        = 4*1000*1000
        expected = fromIntegral n*(fromIntegral n-1)/2 * 0.1
    -- Run it
    b <- eval ddpDotProduct n
    unboundKernel "output" [] $ liftIO $ putStrLn $ concat
      [ "RESULT: ", show b
      , " EXPECTED: ", show expected
      , if b == expected then " -- ok" else " -- WRONG!"
      ]

  where
    rtable = DDP.__remoteTable
           . DDP_Slice.__remoteTable
