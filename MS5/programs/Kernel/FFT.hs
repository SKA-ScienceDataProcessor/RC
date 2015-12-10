{-# LANGUAGE DataKinds #-}

module Kernel.FFT where

import Flow.Builder
import Flow.Vector
import Flow.Halide
import Flow.Kernel

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

fftCreatePlans :: GridPar -> Kernel Tag
fftCreatePlans _ = mappingKernel "fftPlans" Z planRepr $ \_ _ ->
  return nullVector

ifftKern :: GridPar -> UVDom -> Flow Tag -> Flow UVGrid -> Kernel Image
ifftKern gp uvdom = const (halideKernel1 "ifftKern" (uvgRepr uvdom) (facetRepr gp) kern_fft)
foreign import ccall unsafe kern_fft :: HalideFun '[UVGRepr] ImageRepr
