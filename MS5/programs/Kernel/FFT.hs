
module Kernel.FFT where

import Flow.Builder
import Flow.Halide

import Kernel.Data

fftCreatePlans :: GridPar -> Kernel Tag
fftCreatePlans _ = halideKernel0 "fftPlans" planRepr undefined

ifftKern :: GridPar -> Flow Tag -> Flow UVGrid -> Kernel Image
ifftKern gp = halideKernel2 "ifftKern" planRepr (uvgRepr gp) (imageRepr gp) undefined
