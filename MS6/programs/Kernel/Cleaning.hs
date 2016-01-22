{-# LANGUAGE DataKinds #-}

module Kernel.Cleaning where

import Data.Int

import Flow.Builder
import Flow.Halide

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

-- | PSF visibility update kernel
psfVisKernel :: TDom -> Flow Vis -> Kernel Vis
psfVisKernel tdom = halideKernel1 "psfvis" (rawVisRepr tdom) (rawVisRepr tdom) $
  kern_psf_vis
foreign import ccall unsafe kern_psf_vis
  :: HalideFun '[RawVisRepr] RawVisRepr
