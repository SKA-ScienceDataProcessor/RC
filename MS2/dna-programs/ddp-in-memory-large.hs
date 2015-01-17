{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Control.Monad ( forM )

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

    -- Chunk into steps
    let maxWorkSize = 10000000 -- Maxmimum elements to work on at a time
        stepCount = fromIntegral $ ((size-1) `div` maxWorkSize)+1
    liftIO $ putStrLn $ "Step count: " ++ show stepCount
    stepResults <- forM (scatterSlice stepCount (Slice 0 size)) $ \slice -> do

      -- Chunk & send out. Note that we are re-starting the actors for
      -- every iteration. This is hardly ideal.
      res   <- selectMany (Frac 1) (NNodes 1) [UseLocal]
      shell <- startGroup res Failout $(mkStaticClosure 'ddpProductSlice)
      let slicer n = scatterSlice n slice
      broadcastParamSlice slicer shell

      -- Gather
      partials <- delayGroup shell
      x <- duration "collecting vectors" $ gather partials (+) 0
      liftIO $ putStrLn $ "Partial sum: " ++ show x
      return x

    liftIO $ removeFile fname
    return $ sum stepResults

main :: IO ()
main =  dnaRun rtable $ do

    -- Size of vectors. We can pre-compute the expected result.
    let n = 800000000
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
