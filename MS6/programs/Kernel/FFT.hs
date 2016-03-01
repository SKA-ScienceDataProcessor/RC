{-# LANGUAGE DataKinds #-}

module Kernel.FFT where

import Flow.Builder
import Flow.Halide

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

fftKern :: GridPar -> Flow Image -> Kernel FullUVGrid
fftKern gp = halideKernel1 "fftKern" (imageRepr gp) (fullUVGRepr gp) kern_fft
foreign import ccall unsafe kern_fft :: HalideFun '[ImageRepr] FullUVGRepr

ifftKern :: GridPar -> UVDom -> Flow UVGrid -> Kernel Image
ifftKern gp uvdom = halideKernel1 "ifftKern" (uvgRepr uvdom) (facetRepr gp) kern_ifft
foreign import ccall unsafe kern_ifft :: HalideFun '[UVGRepr] ImageRepr
