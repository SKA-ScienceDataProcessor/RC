{-# LANGUAGE DataKinds #-}

module Kernel.Gridder where

import Data.Int

import Flow.Builder
import Flow.Domain
import Flow.Halide

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

-- | Gridder grid initialisation. This is separate because
-- "gridKernel" requires an input to work.
gridInit :: GridPar -> Kernel UVGrid
gridInit gp = halideKernel0 "gridInit" (uvgRepr gp) kern_init
foreign import ccall unsafe kern_init :: HalideFun '[] UVGRepr

-- | Gridder kernel binding
gridKernel :: GridPar -> GCFPar -> Domain Bins
           -> Flow Vis -> Flow GCFs -> Flow UVGrid
           -> Kernel UVGrid
-- TODO: Right now the grid gets copied in, but we would actually like
-- it to be updated!
gridKernel gp gcfp dh =
  halideKernel2Write "gridKernel" (visRepr dh) (gcfsRepr dh gcfp) (uvgRepr gp) $
  kern_scatter `halideBind` gridTheta gp `halideBind` fromIntegral (gridHeight gp)
foreign import ccall unsafe kern_scatter
  :: HalideBind Double (HalideBind Int32 (HalideFun '[VisRepr, GCFsRepr] UVGRepr))
