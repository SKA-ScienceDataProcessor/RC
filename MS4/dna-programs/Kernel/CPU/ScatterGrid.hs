module Kernel.CPU.ScatterGrid where

import Data.Complex
import Foreign.C
import Foreign.Ptr

type PD = Ptr Double
type PCD = Ptr (Complex Double)

type CPUGridderType =
     CDouble   -- scale
  -> CDouble   -- wstep
  -> CInt      -- # of baselines
  -> Ptr CInt  -- GCF supports vector
  -> PCD       -- grid
  -> Ptr PCD   -- GCF layers pointer
  -> Ptr PD    -- baselines' uvw data
  -> Ptr PCD   -- baselines' vis data
  -> CInt      -- length of baselines vectors
  -> CInt      -- grid pitch
  -> CInt      -- grid size
  -> IO ()

foreign import ccall "& gridKernelCPUHalfGCF" gridKernelCPUHalfGCF_ptr :: FunPtr CPUGridderType
foreign import ccall "& gridKernelCPUFullGCF" gridKernelCPUFullGCF_ptr :: FunPtr CPUGridderType

foreign import ccall "dynamic" mkCPUGridderFun :: FunPtr CPUGridderType -> CPUGridderType
