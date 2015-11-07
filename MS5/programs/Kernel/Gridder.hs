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
gridInit :: Domain Range -> Domain Range -> Kernel UVGrid
gridInit ydom xdom = halideKernel0 "gridInit" (uvgRepr ydom xdom) kern_init
foreign import ccall unsafe kern_init :: HalideFun '[] UVGRepr

-- | Gridder kernel binding
gridKernel :: GridPar -> GCFPar -> Domain Range -> Domain Range -> Domain Bins
           -> Flow Vis -> Flow GCFs -> Flow UVGrid
           -> Kernel UVGrid
gridKernel gp gcfp ydom xdom dh =
  halideKernel2Write "gridKernel" (visRepr dh) (gcfsRepr dh gcfp) (uvgRepr ydom xdom) $
  kern_scatter `halideBind` gridTheta gp `halideBind` fromIntegral (gridHeight gp)
foreign import ccall unsafe kern_scatter
  :: HalideBind Double (HalideBind Int32 (HalideFun '[VisRepr, GCFsRepr] UVGRepr))
