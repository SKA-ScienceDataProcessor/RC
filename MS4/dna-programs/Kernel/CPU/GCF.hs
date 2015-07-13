module Kernel.CPU.GCF (kernel) where

import Foreign.C
import Foreign.Ptr
import Data.Complex

import Kernel.CPU.FFT
import Data

foreign import ccall mkGCFLayer ::
     FftPlan
  -> Ptr (Complex Double) -- destination buffer
  -> Ptr (Complex Double) -- work buffer
  -> CInt                 -- support size to extract
  -> CInt                 -- maximum support size
  -> CInt                 -- lead dim padding
  -> Double               -- Theta/2
  -> Double               -- w
  -> IO FftPlan

kernel ::
     GridPar
  -> GCFPar
  -> Vis
  -> IO GCFSet
kernel _gp _gcfp _vis = do
  -- Nonsense stub call
  mkGCFLayer nullPtr nullPtr nullPtr 0 0 0 0 0
  return undefined
