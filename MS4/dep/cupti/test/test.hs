
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad ( forM_ )

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as A
import qualified Data.Array.Accelerate.CUDA as CUDA

import qualified Data.Vector.Storable as S

import Data.Time.Clock

import Profiling.CUDA.Activity
import Profiling.CUDA.Metrics

import Text.Printf

accelerateDDP :: S.Vector Double -> S.Vector Double -> IO Double
accelerateDDP va vb
    = do let sh = A.Z A.:. S.length va
             va' = A.fromVectors sh ((), va) :: A.Vector Double
             vb' = A.fromVectors sh ((), vb) :: A.Vector Double
             res = CUDA.run (A.fold (+) 0 (A.zipWith (*) (A.use va') (A.use vb')))
         return $! head $ A.toList res

main :: IO ()
main = do

  let size = 1000 * 1000 * 30
      in1d, in2d :: S.Vector Double
      !in1d = S.replicate size 10
      !in2d = S.generate size (\i -> 0.1 * fromIntegral i)

  let metricNames
        -- = ["flop_count_dp", "flop_count_dp_add", "flop_count_dp_mul", "flop_count_dp_fma"]
        -- = ["flop_count_sp", "flop_count_sp_add", "flop_count_sp_mul", "flop_count_sp_fma"]
        -- = ["inst_fp_64", "inst_fp_32"]
        -- = ["inst_executed", "inst_issued"]
        = ["inst_integer"]
  cuptiMetricsInit metricNames

  -- Profile
  beginTime <- getCurrentTime
  cuptiEnable
  cuptiMetricsEnable
  !_ <- accelerateDDP in1d in2d
  cuptiMetricsDisable
  cuptiDisable
  metrics <- cuptiGetMetrics
  endTime <- getCurrentTime

  -- Read stats
  let locs = [(CUptiHost)..CUptiArray]
      locPairs = [ (from, to) | from <- locs, to <- locs ]
  cuptiFlush
  memsetTime <- cuptiGetMemsetTime
  memcpyTimes <- mapM (uncurry cuptiGetMemcpyTime) locPairs
  kernelTime <- cuptiGetKernelTime
  overheadTime <- cuptiGetOverheadTime
  memsetBytes <- cuptiGetMemsetBytes
  memcpyBytess <- mapM (uncurry cuptiGetMemcpyBytes) locPairs

  -- Show them
  let time = toRational $ endTime `diffUTCTime` beginTime
  putStrLn $ printf "Time:          %9d ms" (floor (time * 1000) :: Int)
  putStrLn $ printf "Memset:        %9d ms, %6d KB" (memsetTime `div` 1000000) (memsetBytes `div` 1000)
  putStrLn $ printf "Memcpy:        %9d ms, %6d KB" (sum memcpyTimes `div` 1000000) (sum memcpyBytess `div` 1000)
  putStrLn $ printf "Kernel:        %9d ms" (kernelTime `div` 1000000)
  putStrLn $ printf "Overhead time: %9d ms" (overheadTime `div` 1000000)

  putStrLn ""
  forM_ (zip metricNames metrics) $ \(name, v) ->
    putStrLn $ printf "Metric %s: %d" name v
