
{-# LANGUAGE BangPatterns, ScopedTypeVariables, CPP #-}

module Main where

import Control.Monad

import Profiling.Linux.Perf.Stat
import Data.Time.Clock
import Text.Printf

import qualified Data.Vector.Storable as V

import Foreign.Ptr
import Foreign.C.Types

import System.IO.Unsafe

foreign import ccall unsafe "sse_ddp"
  c_sse_ddp :: Ptr CFloat -> Ptr CFloat -> CInt -> IO Float
foreign import ccall unsafe "sse_ddp_d"
  c_sse_ddp_d :: Ptr CFloat -> Ptr CFloat -> CInt -> IO Double

foreign import ccall unsafe "avx_ddp"
  c_avx_ddp :: Ptr CFloat -> Ptr CFloat -> CInt -> IO Float
foreign import ccall unsafe "avx_ddp_d"
  c_avx_ddp_d :: Ptr CFloat -> Ptr CFloat -> CInt -> IO Double

foreign import ccall unsafe "omp_pi"
  c_omp_pi :: IO Double

sseDDP :: V.Vector Float -> V.Vector Float -> Float
sseDDP in1 in2 =
  unsafePerformIO $
  V.unsafeWith in1 $ \pin1 ->
  V.unsafeWith in2 $ \pin2 -> do
    let cnt = V.length in1
    c_sse_ddp (castPtr pin1) (castPtr pin2) (fromIntegral cnt)

sseDDPd :: V.Vector Double -> V.Vector Double -> Double
sseDDPd in1 in2 =
  unsafePerformIO $
  V.unsafeWith in1 $ \pin1 ->
  V.unsafeWith in2 $ \pin2 -> do
    let cnt = V.length in1
    c_sse_ddp_d (castPtr pin1) (castPtr pin2) (fromIntegral cnt)

avxDDP :: V.Vector Float -> V.Vector Float -> Float
avxDDP in1 in2 =
  unsafePerformIO $
  V.unsafeWith in1 $ \pin1 ->
  V.unsafeWith in2 $ \pin2 -> do
    let cnt = V.length in1
    c_avx_ddp (castPtr pin1) (castPtr pin2) (fromIntegral cnt)

avxDDPd :: V.Vector Double -> V.Vector Double -> Double
avxDDPd in1 in2 =
  unsafePerformIO $
  V.unsafeWith in1 $ \pin1 ->
  V.unsafeWith in2 $ \pin2 -> do
    let cnt = V.length in1
    c_avx_ddp_d (castPtr pin1) (castPtr pin2) (fromIntegral cnt)

main :: IO ()
main = do

  group <- perfEventOpen
           [ PerfDesc (PERF_TYPE_HARDWARE PERF_COUNT_HW_CPU_CYCLES)
           , PerfDesc (PERF_TYPE_HARDWARE PERF_COUNT_HW_INSTRUCTIONS)
           , PfmDesc "FP_COMP_OPS_EXE:X87"
           , PfmDesc "FP_COMP_OPS_EXE:SSE_FP_SCALAR_SINGLE"
           , PfmDesc "FP_COMP_OPS_EXE:SSE_SCALAR_DOUBLE"
           , PfmDesc "FP_COMP_OPS_EXE:SSE_PACKED_SINGLE"
           , PfmDesc "FP_COMP_OPS_EXE:SSE_FP_PACKED_DOUBLE"
           , PfmDesc "SIMD_FP_256:PACKED_SINGLE"
           , PfmDesc "SIMD_FP_256:PACKED_DOUBLE"
           ]

  let size = 1000 * 1000 * 10
      in1f, in2f :: V.Vector Float
      !in1f = V.replicate size 10
      !in2f = V.generate size (\i -> 0.1 * fromIntegral i)
      in1d, in2d :: V.Vector Double
      !in1d = V.replicate size 10
      !in2d = V.generate size (\i -> 0.1 * fromIntegral i)

      expected :: RealFloat a => a
      expected = fromIntegral size * fromIntegral size / 2
      check :: (Show a, RealFloat a) => a -> IO ()
      check v =
        when (abs (v - expected) > expected / 10) $
          putStrLn $ "Wrong result: " ++ show v ++ " != " ++ show (expected :: Double)

  let tests = [ check $ V.sum $ V.zipWith (*) in1f in2f
              , check $ V.sum $ V.zipWith (*) in1d in2d
              , check $ sseDDP in1f in2f
              , check $ sseDDPd in1d in2d
              , check $ avxDDP in1f in2f
              , check $ avxDDPd in1d in2d
              , void $ c_omp_pi
              ]

  -- Warm up, and prevent full laziness
  replicateM_ 2 $ sequence tests

  perfEventEnable group
  forM_ tests $ \test -> do

    -- Run loop
    let repeats = 10
    beginTime <- getCurrentTime
    perfEventEnable group
    begin <- perfEventRead group
    replicateM_ repeats $ test
    end <- perfEventRead group
    perfEventDisable group
    endTime <- getCurrentTime
    let expectedOps = size * 2 * repeats

    -- Show stats
    let (cycles:instrs:
         x87:
         scalarSingle:scalarDouble:
         packedSingle:packedDouble:
         avx256Single:avx256Double:
         _)
             = zipWith perfEventDiff end begin
        time = toRational $ endTime `diffUTCTime` beginTime
        perTime :: PerfStatCount -> Double
        perTime ps = 1000 * fromIntegral (psValue ps) / fromIntegral (psTimeRunning ps)
    putStrLn $ unlines $ map concat
      [ [ printf "Time:          %10d ms" (floor (time * 1000000) :: Int) ]
      , [ printf "Time Running:  %10d ms" (psTimeRunning cycles `div` 1000) ]
      , [ printf "Time Enabled:  %10d ms" (psTimeEnabled cycles `div` 1000) ]
      , [ printf "Cycles:        %10d     - %8.2f MHz"   (psValue cycles) (perTime cycles) ]
      , [ printf "Instructions:  %10d     - %8.2f MHz"   (psValue instrs) (perTime instrs) ]
      , [ printf "x87:           %10d OPs - %8.2f MOP/s" (psValue x87)    (perTime x87)    ]
      , [ printf "Scalar Single: %10d OPs - %8.2f MOP/s" (psValue scalarSingle) (perTime scalarSingle) ]
      , [ printf "Scalar Double: %10d OPs - %8.2f MOP/s" (psValue scalarDouble) (perTime scalarDouble) ]
      , [ printf "SSE Single:    %10d OPs - %8.2f MOP/s" (4*psValue packedSingle) (4*perTime packedSingle) ]
      , [ printf "SSE Double:    %10d OPs - %8.2f MOP/s" (2*psValue packedDouble) (2*perTime packedDouble) ]
      , [ printf "AVX Single:    %10d OPs - %8.2f MOP/s" (8*psValue avx256Single) (8*perTime avx256Single) ]
      , [ printf "AVX Double:    %10d OPs - %8.2f MOP/s" (4*psValue avx256Double) (4*perTime avx256Double) ]
      , [ printf "Reference:     %10d OPs - %8.2f MOP/s" (expectedOps) (fromRational (fromIntegral expectedOps / time / 1000000) :: Float) ]
      ]

  perfEventClose group
