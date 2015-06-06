{-# LANGUAGE CPP #-}

module CPUGridder where

import Data.Complex
import Foreign.C
import Foreign.Ptr
import OskarBinReaderFFI

type CPUGridderType = Double -> CDouble -> CInt -> Ptr BlWMap -> Ptr (Complex Double) -> Ptr (Ptr (Complex Double)) -> Ptr CDouble -> Ptr (Complex Double) -> IO ()

#define __CPU_GRIDDER(fun) \
foreign import ccall "& fun" fun/**/_ptr :: FunPtr CPUGridderType

foreign import ccall "dynamic" mkCPUGridderFun :: FunPtr CPUGridderType -> CPUGridderType

__CPU_GRIDDER(gridKernelCPUHalfGCF)
__CPU_GRIDDER(gridKernelCPUHalfGCFPerm)
__CPU_GRIDDER(gridKernelCPUFullGCF)
__CPU_GRIDDER(gridKernelCPUFullGCFPerm)

foreign import ccall fft_inplace_even :: Ptr (Complex Double) -> IO ()
foreign import ccall normalizeAndExtractPolarizationCPU :: CInt -> Ptr (Complex Double) -> Ptr (Complex Double) -> IO ()
