{-# LANGUAGE DataKinds #-}

module Kernel.FFT where

import Flow.Builder
import Flow.Vector
import Flow.Halide

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

fftCreatePlans :: GridPar -> Kernel Tag
fftCreatePlans _ = kernel "fftPlans" Z planRepr $ \_ _ ->
  return nullVector

ifftKern :: GridPar -> Flow Tag -> Flow UVGrid -> Kernel Image
ifftKern gp ft fu = timeKernel "ifftKern" (const (halideKernel1 "ifftKern" (uvgRepr gp) (imageRepr gp) kern_fft) ft fu)
foreign import ccall unsafe kern_fft :: HalideFun '[UVGRepr] ImageRepr
