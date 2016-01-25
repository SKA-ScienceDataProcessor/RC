{-# LANGUAGE DataKinds #-}

module Kernel.Cleaning where

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

-- | Cleaning kernel binding, returning the model
cleanModel :: GridPar -> CleanPar -- ^ Configuration
           -> Flow Image          -- ^ PSF
           -> Flow Image          -- ^ Image to clean
           -> Kernel Image        -- ^ New model
cleanModel gpar cpar =
  halideKernel2 "clean model" (imageRepr gpar) (imageRepr gpar) (imageRepr gpar) $
  kern_hogbom_model `halideBind` cleanGain cpar
                    `halideBind` cleanThreshold cpar
foreign import ccall unsafe kern_hogbom_model
  :: HalideBind Double (HalideBind Double (HalideFun '[ImageRepr, ImageRepr] ImageRepr))

-- | Cleaning kernel binding, returning the residual
cleanResidual :: GridPar -> CleanPar -- ^ Configuration
              -> Flow Image          -- ^ PSF
              -> Flow Image          -- ^ Image to clean
              -> Kernel Image        -- ^ Residual
cleanResidual gpar cpar =
  halideKernel2 "clean residual" (imageRepr gpar) (imageRepr gpar) (imageRepr gpar) $
  kern_hogbom_residual `halideBind` cleanGain cpar
                       `halideBind` cleanThreshold cpar
foreign import ccall unsafe kern_hogbom_residual
  :: HalideBind Double (HalideBind Double (HalideFun '[ImageRepr, ImageRepr] ImageRepr))
