{-# LANGUAGE BangPatterns #-}

module Kernel.CPU.FFT where

import Data.IORef
import Foreign.C
import Foreign.Ptr

import Data
import Vector

data FftPlanOpaque
type FftPlan = Ptr FftPlanOpaque

foreign import ccall unsafe fftInitThreading :: IO ()
foreign import ccall "fft_inplace_even" fftInplaceEven ::
     FftPlan
  -> CInt    -- Sign: -1 direct C2C, 0 - R2C, +1 inverse C2C, +2 C2R
  -> Ptr ()  -- Data ptr
  -> CInt    -- Size. Should be even!
  -> CInt    -- Pitch.
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

dftGeneral :: CInt -> Proj -> Inj -> IORef DftPlans -> GridPar -> Ptr () -> IO ()
dftGeneral sign prj inj plans gp datap = do
    plns <- readIORef plans
    plan' <- fftInplaceEven (prj plns) sign datap (fi height) (fi pitch)
    writeIORef plans (inj plns plan')
  where
    fi = fromIntegral
    height = gridHeight gp
    pitch = gridPitch gp

fromVec :: Vector a -> Ptr ()
fromVec (CVector _ p) = castPtr p
fromVec _ = error "Wrong CPU FFT vector location."

dftKernel :: IORef DftPlans -> Image -> IO UVGrid
dftKernel pref (Image par pad d) = do
  dftGeneral 0 dftPlan (\plns p -> plns {dftPlan = p}) pref par (fromVec d)
  return (UVGrid par pad $ castVector d)

dftIKernel :: IORef DftPlans -> UVGrid -> IO Image
dftIKernel pref (UVGrid par pad d) = do
  dftGeneral 2 dftIPlan (\plns p -> plns {dftIPlan = p}) pref par (fromVec d)
  return (Image par pad $ castVector d)
