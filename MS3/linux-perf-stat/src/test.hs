
{-# LANGUAGE BangPatterns, ScopedTypeVariables, CPP #-}

module Main where

import Control.Monad
import Control.Monad.Primitive

import Profiling.Linux.Perf.Stat
import Data.Time.Clock
import Data.Word
import Text.Printf

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import Foreign.Ptr
import Foreign.Storable
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

  group2 <- perfEventOpen
           [ PfmDesc "OFFCORE_RESPONSE_0:ANY_DATA:LLC_MISS_LOCAL"
           ]

  let size = 1000 * 1000 * 100
      repeats = 10

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

  -- Prepare mutable versions for transfer test
  inf <- V.thaw in1f
  outf <- VM.new size :: IO (VM.IOVector Float)

  let ddpOps = size * 2
      sizeV :: Storable a => V.Vector a -> Int
      sizeV vec = V.length vec * sizeOf (V.head vec)

      tests = [ (ddpOps, sizeV in1f + sizeV in2f,
                 check $ V.sum $ V.zipWith (*) in1f in2f)
              , (ddpOps, sizeV in1d + sizeV in2d,
                 check $ V.sum $ V.zipWith (*) in1d in2d)
              , (ddpOps, sizeV in1f + sizeV in2f,
                 check $ sseDDP in1f in2f)
              , (ddpOps, sizeV in1d + sizeV in2d,
                 check $ sseDDPd in1d in2d)
              , (ddpOps, sizeV in1f + sizeV in2f,
                 check $ avxDDP in1f in2f)
              , (ddpOps, sizeV in1d + sizeV in2d,
                 check $ avxDDPd in1d in2d)
              , (1000000 * 6, 0,
                 void $ c_omp_pi)
              , (0, sizeV in1f,
                 VM.copy inf outf)
              ]

  -- Warm up, and prevent full laziness
  replicateM_ 2 $ forM_ tests $ \(_,_,t) -> t

  perfEventEnable group
  forM_ tests $ \(ops, mem, test) -> do

    -- Run loop
    beginTime <- getCurrentTime
    perfEventEnable group
    perfEventEnable group2
    begin <- perfEventRead group
    begin2 <- perfEventRead group2
    replicateM_ repeats $ test
    end <- perfEventRead group
    end2 <- perfEventRead group2
    perfEventDisable group
    perfEventDisable group2
    endTime <- getCurrentTime
    let expectedOps = repeats * ops
        expectedMem = repeats * mem
        cacheLine = 64

    -- Show stats
    let (cycles:instrs:
         x87:
         scalarSingle:scalarDouble:
         packedSingle:packedDouble:
         avx256Single:avx256Double:
         _)
             = zipWith perfEventDiff end begin
        (mem:
         _)
             = zipWith perfEventDiff end2 begin2
        time = toRational $ endTime `diffUTCTime` beginTime

        total :: PerfStatCount -> Word64
        total ps = psValue ps * (1024 * psTimeEnabled ps `div` psTimeRunning ps) `div` 1024
        perTime :: PerfStatCount -> Double
        perTime ps = 1000 * fromIntegral (psValue ps) / fromIntegral (psTimeRunning ps)
        timeStats :: PerfStatCount -> String
        timeStats ps = printf "[%4d/%4d ms %d%%]"
                           (psTimeRunning ps `div` 1000000)
                           (psTimeEnabled ps `div` 1000000)
                           (psTimeRunning ps * 100 `div` psTimeEnabled ps)
    putStrLn $ unlines $ map concat
      [ [ printf "Time:          %11d us" (floor (time * 1000000) :: Int) ]
      , [ printf "Cycles:        %11d     - %9.2f MHz   %20s" (total cycles) (perTime cycles) (timeStats cycles) ]
      , [ printf "Instructions:  %11d     - %9.2f MHz   %20s" (total instrs) (perTime instrs) (timeStats instrs) ]
      , [ printf "x87:           %11d OPs - %9.2f MOP/s %20s" (total x87)    (perTime x87)    (timeStats x87) ]
      , [ printf "Scalar Single: %11d OPs - %9.2f MOP/s %20s" (total scalarSingle) (perTime scalarSingle) (timeStats scalarSingle) ]
      , [ printf "Scalar Double: %11d OPs - %9.2f MOP/s %20s" (total scalarDouble) (perTime scalarDouble) (timeStats scalarDouble) ]
      , [ printf "SSE Single:    %11d OPs - %9.2f MOP/s %20s" (4*total packedSingle) (4*perTime packedSingle) (timeStats packedSingle) ]
      , [ printf "SSE Double:    %11d OPs - %9.2f MOP/s %20s" (2*total packedDouble) (2*perTime packedDouble) (timeStats packedDouble) ]
      , [ printf "AVX Single:    %11d OPs - %9.2f MOP/s %20s" (8*psValue avx256Single) (8*perTime avx256Single) (timeStats avx256Single) ]
      , [ printf "AVX Double:    %11d OPs - %9.2f MOP/s %20s" (4*psValue avx256Double) (4*perTime avx256Double) (timeStats avx256Double)]
      , [ printf "Reference:     %11d OPs - %9.2f MOP/s" (expectedOps) (fromRational (fromIntegral expectedOps / time / 1000000) :: Float) ]
      , [ printf "L3 Read:       %11d B   - %9.2f MB/s  %20s"  (cacheLine*total mem) (fromIntegral cacheLine*perTime mem) (timeStats mem)]
      , [ printf "Reference:     %11d B   - %9.2f MB/s"  (expectedMem) (fromRational (fromIntegral expectedMem / time / 1000000) :: Float )]
        ]

  perfEventClose group
