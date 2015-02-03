{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import DNA.Channel.File (readDataMMap)
import DNA

import DDP
import DDP_Slice_Accelerate


----------------------------------------------------------------
-- Distributed dot product
--
-- Note that actors which do not spawn actors on other nodes do not
-- receive CAD.
----------------------------------------------------------------

ddpCollector :: CollectActor Double Double
ddpCollector = collectActor
    (\s a -> return $! s + a)
    (return 0)
    (return)

remotable [ 'ddpCollector
          ]

-- | Actor for calculating dot product
ddpDotProduct :: Actor Slice Double
ddpDotProduct = actor $ \size -> do
    res <- selectMany (Frac 1) (NNodes 1) [UseLocal]
    r   <- select Local (N 0)
    shell <- startGroup res Failout $(mkStaticClosure 'ddpProductSlice)
    shCol <- startCollector r $(mkStaticClosure 'ddpCollector)
    sendParam size $ broadcast shell
    connect shell shCol
    res <- delay Remote shCol
    await res

main :: IO ()
main = dnaRun rtable $ do
    let n        = 10*1000*1000
        expected = fromIntegral n*(fromIntegral n-1)/2 * 0.1
    -- Show configuration
    nodes <- groupSize
    let size = n * 8; sizePerNode = size `div` fromIntegral nodes
    liftIO $ putStrLn $ concat
        [ "Data: ", show (size `div` 1000000), " MB total, "
        , show (sizePerNode `div` 1000000), " MB per node"]
    -- Run it
    b <- eval ddpDotProduct (Slice 0 n)
    liftIO $ putStrLn $ concat
      [ "RESULT: ", show b
      , " EXPECTED: ", show expected
      , if b == expected then " -- ok" else " -- WRONG!"
      ]
  where
    rtable = DDP.__remoteTable
           . DDP_Slice_Accelerate.__remoteTable
           . Main.__remoteTable
