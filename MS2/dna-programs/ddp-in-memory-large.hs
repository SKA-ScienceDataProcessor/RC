{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Data.Int

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
ddpDotProduct :: Actor Int64 Double
ddpDotProduct = actor $ \size -> do
    -- Find how into how  many chunks we need to split vector
    let nChunk = fromIntegral $ size `div` (1000*1000)
    -- Chunk & send out
    res   <- selectMany (Frac 1) (NNodes 1) [UseLocal]
    shell <- startGroupN res Failout nChunk $(mkStaticClosure 'ddpProductSlice)
    sendParam (Slice 0 size) shell
    -- Collect results
    partials <- delayGroup shell
    x <- duration "collecting vectors" $ gather partials (+) 0
    return x

main :: IO ()
main =  dnaRun rtable $ do
    -- Size of vectors. We can pre-compute the expected result.
    let n        = 50*1000*1000
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
