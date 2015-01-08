{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Data.Int

import System.Directory ( removeFile )

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

    -- Run generator & delay
    resG   <- select Local (N 0)
    shellG <- startActor resG $(mkStaticClosure 'ddpGenerateVector)
    sendParam size shellG
    fname <- duration "generate" . await =<< delay Remote shellG

    -- Chunk & send out
    res   <- selectMany (Frac 1) (NNodes 1) [UseLocal]
    shell <- startGroup res Failout $(mkStaticClosure 'ddpProductSlice)
    let slicer n = map ((,) fname) $ scatterSlice n (Slice 0 size)
    broadcastParamSlice slicer shell

    partials <- delayGroup shell
    x <- duration "collecting vectors" $ gather partials (+) 0

    liftIO $ removeFile fname
    return x

main :: IO ()
main =  dnaRun rtable $ do

    -- Size of vectors. We can pre-compute the expected result.
    let n = 20000000
        ns = n `div` 4
        seg_sum i = (2*i*ns + ns - 1) * ns `div` 2 * (i+1)
        expected = fromIntegral $ sum (map seg_sum [0,1,2,3]) `div` 10

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
