{-# LANGUAGE CPP #-}

module CPUGridder where

import Data.Complex
import OskarBinReaderFFI

typedef CPUGridderType = Double -> Ptr BlWMap -> Ptr (Complex Double) -> Ptr (Ptr (Complex Double)) -> Ptr Double -> Ptr (Complex Double) -> IO ()

#define __CPU_GRIDDER(fun) \
foreign import ccall "& fun" fun/**/_ptr :: FunPtr CPUGridderType

foreign import ccall "dynamic" mkCPUGridderFun :: FunPtr CPUGridderType -> CPUGridderType

__CPU_GRIDDER(gridKernelCPUHalfGCF)
__CPU_GRIDDER(gridKernelCPUHalfGCFPerm)
__CPU_GRIDDER(gridKernelCPUFullGCF)
__CPU_GRIDDER(gridKernelCPUFullGCFPerm)
