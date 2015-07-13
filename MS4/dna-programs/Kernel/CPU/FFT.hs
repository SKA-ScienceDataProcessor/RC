module Kernel.CPU.FFT where

import Foreign.C
import Foreign.Ptr
import Data.Complex

foreign import ccall unsafe fftInitThreading :: IO ()
foreign import ccall "fft_inplace_even" fftInplaceEven :: Ptr (Complex Double) -> CInt -> CInt -> IO ()
