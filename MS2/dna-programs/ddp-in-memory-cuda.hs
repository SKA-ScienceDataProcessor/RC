{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import DNA.Channel.File (readDataMMap)
import DNA

import DDP
import DDP_Slice_CUDA


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
    -- Vector size:
    --
    -- > 20e4 doubles per node = 160 MB per node
    -- > 4 nodes
    let n        = 20*1000*1000
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
           . DDP_Slice_CUDA.__remoteTable
           . Main.__remoteTable
