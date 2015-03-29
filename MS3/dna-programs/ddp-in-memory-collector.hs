{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import DNA.Channel.File (readDataMMap)
import DNA

import DDP
import DDP_Slice


----------------------------------------------------------------
-- Distributed dot product
----------------------------------------------------------------

-- | Actor for calculating dot product
ddpDotProduct :: Actor Slice Double
ddpDotProduct = actor $ \size -> do
    shell <- startGroup (Frac 1) (NNodes 1) $ do
        useLocal
        return $(mkStaticClosure 'ddpProductSlice)
    shCol <- startCollector (N 0) $ do
        useLocal
        return $(mkStaticClosure 'ddpCollector)
    sendParam size $ broadcast shell
    connect shell shCol
    res <- delay Remote shCol
    await res

main :: IO ()
main = dnaRun rtable $ do
    -- Vector size:
    --
    -- > 100e4 doubles per node = 800 MB per node
    -- > 4 nodes
    let n        = 400*1000*100
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
