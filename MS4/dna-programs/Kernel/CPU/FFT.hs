{-# LANGUAGE BangPatterns #-}

module Kernel.CPU.FFT where

import Data.IORef
import Foreign.C
import Foreign.Ptr
import Data.Complex

import Data
import Vector

data FftPlanOpaque
type FftPlan = Ptr FftPlanOpaque

foreign import ccall unsafe fftInitThreading :: IO ()
foreign import ccall "fft_inplace_even" fftInplaceEven ::
     FftPlan
  -> CInt                  -- Sign: -1 direct, +1 inverse
  -> Ptr (Complex Double)  -- Data ptr
  -> CInt                  -- Size. Should be even!
  -> CInt                  -- Pitch.
  -> IO FftPlan
foreign import ccall "fftw_destroy_plan" fftDestroyPlan :: FftPlan -> IO ()

data DftPlans = DftPlans
  { dftPlan :: FftPlan
  , dftIPlan :: FftPlan
  }

dftPrepare :: IORef DftPlans -> GridPar -> IO ()
dftPrepare plans _gridp = writeIORef plans (DftPlans nullPtr nullPtr)

dftClean :: IORef DftPlans -> IO ()
dftClean plans = do
  DftPlans plan iplan <- readIORef plans
  fftDestroyPlan plan
  fftDestroyPlan iplan
  writeIORef plans (DftPlans nullPtr nullPtr)

type Proj = DftPlans -> FftPlan
type Inj = DftPlans -> FftPlan -> DftPlans

dftGeneral :: CInt -> Proj -> Inj -> IORef DftPlans -> UVGrid -> IO UVGrid
dftGeneral sign prj inj plans g@(UVGrid ip _ (CVector _ datap)) = do
    plns <- readIORef plans
    plan' <- fftInplaceEven (prj plns) sign datap (fi height) (fi pitch)
    writeIORef plans (inj plns plan')
    return g
  where
    fi = fromIntegral
    height = gridHeight ip
    pitch = gridPitch ip
dftGeneral _ _ _ _ _ = error "Wrong grid location in CPU FFT."

dftKernel :: IORef DftPlans -> UVGrid -> IO UVGrid
dftKernel = dftGeneral (-1) dftPlan (\plns p -> plns {dftPlan = p})

dftIKernel :: IORef DftPlans -> UVGrid -> IO UVGrid
dftIKernel = dftGeneral 1 dftIPlan (\plns p -> plns {dftIPlan = p})
