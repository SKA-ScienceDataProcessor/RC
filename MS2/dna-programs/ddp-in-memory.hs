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
    res   <- selectMany (Frac 1) (NNodes 1) [UseLocal]
    shell <- startGroup res Failout $(mkStaticClosure 'ddpProductSlice)
    sendParam (Slice 0 size) (broadcast shell)
    -- Collect results
    partials <- delayGroup shell
    x <- duration "collecting vectors" $ gather partials (+) 0
    return x

main :: IO ()
main =  dnaRun rtable $ do
    -- Size of vectors. We can pre-compute the expected result.
    let n        = 5*1000*1000
        expected = fromIntegral n*(fromIntegral n-1)/2 * 0.1
    -- Run it
    b <- eval ddpDotProduct n
    liftIO $ putStrLn $ concat
      [ "RESULT: ", show b
      , " EXPECTED: ", show expected
      , if b == expected then " -- ok" else " -- WRONG!"
      ]

  where
    rtable = DDP.__remoteTable
           . DDP_Slice.__remoteTable
