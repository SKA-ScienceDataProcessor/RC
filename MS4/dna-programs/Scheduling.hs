-- |
-- Helpers for solving scheduling optimizations
module Scheduling where

import Control.Monad
import Data.List
import Data.Function

balancer
    :: Int      -- ^ Number of nodes
    -> [Double] -- ^ Full execution times for each freq. channel (T for image × N iteraterion)
    -> [Int]    -- ^ Number of nodes allocated to each channel
balancer n ts = snd $ minimumBy (compare `on` fst) $ do
    -- Generate all possible schedules
    nTail <- replicateM (nCh - 1) [1 .. n - nCh + 1]
    let n0 = n - sum nTail
        ns = n0 : nTail
    guard (n0 > 0)
    -- Return pair of execution time and schedule
    return (maximum $ zipWith (\k t -> t / fromIntegral k) ns ts, ns)
  where
    nCh = length ts
