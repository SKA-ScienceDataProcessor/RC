module Kernel.CPU.FFT where

import Foreign.C
import Foreign.Ptr
import Data.Complex

data FftPlanOpaque
type FftPlan = Ptr FftPlanOpaque

foreign import ccall unsafe fftInitThreading :: IO ()
foreign import ccall "fft_inplace_even" fftInplaceEven :: FftPlan -> Ptr (Complex Double) -> CInt -> CInt -> IO FftPlan
foreign import ccall "fftw_destroy_plan" fftDestroyPlan :: FftPlan -> IO ()
