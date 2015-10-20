
module Kernel.FFT where

import Flow.Builder
import Flow.Vector

import Kernel.Data

fftCreatePlans :: GridPar -> Kernel Tag
fftCreatePlans _ = kernel "fftPlans" Z planRepr $ \_ _ -> return nullVector

ifftKern :: GridPar -> Flow Tag -> Flow UVGrid -> Kernel Image
ifftKern gp = kernel "ifftKern" (planRepr :. uvgRepr gp :. Z) (imageRepr gp) $ \_ _ ->
  return nullVector
