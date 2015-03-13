
{-# LANGUAGE BangPatterns, ScopedTypeVariables, CPP #-}

module Main where

import Control.Monad
import Profiling.Linux.Perf.Stat
import Data.Time.Clock
import Text.Printf

-- This is inspired by:
--
-- http://icl.cs.utk.edu/projects/papi/wiki/PAPITopics:SandyFlops

main :: IO ()
main = do

  group <- perfEventOpen
           [ PerfDesc $ PERF_TYPE_HARDWARE PERF_COUNT_HW_CPU_CYCLES
           , PerfDesc $ PERF_TYPE_HARDWARE PERF_COUNT_HW_INSTRUCTIONS
#ifdef USE_LIBPFM
           , PfmDesc "FP_COMP_OPS_EXE:X87"
           , PfmDesc "FP_COMP_OPS_EXE:SSE_FP_SCALAR_SINGLE"
           , PfmDesc "FP_COMP_OPS_EXE:SSE_SCALAR_DOUBLE"
           --, PfmDesc "FP_COMP_OPS_EXE:SSE_PACKED_SINGLE"
           --, PfmDesc "FP_COMP_OPS_EXE:SSE_FP_PACKED_DOUBLE"
           --, PfmDesc "SIMD_FP_256:PACKED_SINGLE"
#endif
           ]

  let go_f :: Int -> Float -> Float
      go_f !0 !c = c
      go_f !a !c = go_f (a-1) (c + fromIntegral a * fromIntegral a)
      test_f = go_f 100000 0 `seq` return ()

      go_d :: Int -> Double -> Double
      go_d !0 !c = c
      go_d !a !c = go_d (a-1) (c + fromIntegral a * fromIntegral a)
      test_d = go_d 100000 0 `seq` return ()

  -- Warm up
  replicateM_ 10 $ test_d >> test_f

  forM_ [test_f, test_d] $ \test -> do

    -- Run loop
    beginTime <- getCurrentTime
    begin <- perfEventRead group
    test
    end <- perfEventRead group
    endTime <- getCurrentTime

    -- Show stats
    let (cycles:instrs:x87:scalarSingle:scalarDouble:_)
             = zipWith perfEventDiff end begin
        time = toRational $ endTime `diffUTCTime` beginTime
        perTime :: PerfStat -> Double
        perTime ps = 1000 * fromIntegral (psValue ps) / fromIntegral (psTimeRunning ps)
    putStrLn $ unlines $ map concat
      [ [ printf "Time:          %6d ms" (floor (time * 1000000) :: Int) ]
      , [ printf "Time Running:  %6d ms" (psTimeRunning cycles `div` 1000) ]
      , [ printf "Time Enabled:  %6d ms" (psTimeEnabled cycles `div` 1000) ]
      , [ printf "Cycles:        %6d     - %8.2f MHz"   (psValue cycles) (perTime cycles) ]
      , [ printf "Instructions:  %6d     - %8.2f MHz"   (psValue instrs) (perTime instrs) ]
      , [ printf "x87:           %6d OPs - %8.2f MOP/s" (psValue x87)    (perTime x87)    ]
      , [ printf "Scalar Single: %6d OPs - %8.2f MOP/s" (psValue scalarSingle) (perTime scalarSingle) ]
      , [ printf "Scalar Double: %6d OPs - %8.2f MOP/s" (psValue scalarDouble) (perTime scalarDouble) ]
      ]

  perfEventClose group
