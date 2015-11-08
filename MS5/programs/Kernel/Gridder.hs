{-# LANGUAGE DataKinds #-}

module Kernel.Gridder where

import Data.Int

import Flow.Builder
import Flow.Domain
import Flow.Halide
import Flow.Domain

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

-- | Gridder grid initialisation. This is separate because
-- "gridKernel" requires an input to work.
gridInit :: GCFPar -> Domain Range -> Domain Range -> Kernel UVGrid
gridInit gcfp ydom xdom = halideKernel0 "gridInit" (uvgMarginRepr gcfp ydom xdom) kern_init
foreign import ccall unsafe kern_init :: HalideFun '[] UVGRepr

-- | Gridder kernel binding
gridKernel :: GridPar -> GCFPar -> Domain Range -> Domain Range -> Domain Bins
           -> Flow Vis -> Flow GCFs -> Flow UVGrid
           -> Kernel UVGrid
gridKernel gp gcfp ydom xdom dh =
  halideKernel2Write "gridKernel" (visRepr dh) (gcfsRepr dh gcfp) (uvgMarginRepr gcfp ydom xdom) $
  kern_scatter `halideBind` gridTheta gp `halideBind` fromIntegral (gridHeight gp)
foreign import ccall unsafe kern_scatter
  :: HalideBind Double (HalideBind Int32 (HalideFun '[VisRepr, GCFsRepr] UVGMarginRepr))

-- | Gridder grid initialisation, for detiling. Only differs from
-- "gridInit" in the produced data representation, we can even re-use
-- the underlying Halide kernel.
gridInitDetile :: Domain Range -> Domain Range -> Kernel UVGrid
gridInitDetile ydom xdom = halideKernel0 "gridInitDetile" (uvgRepr ydom xdom) kern_init

-- | Grid de-tiling kernel. This simply copies tiled (and possibly
-- overlapped) tiles into a common UV-grid.
gridDetiling :: GCFPar -> (Domain Range, Domain Range) -> (Domain Range, Domain Range)
             -> Flow UVGrid -> Flow UVGrid -> Kernel UVGrid
gridDetiling gcfp (ydom0, xdom0) (ydom1, xdom1) =
  halideKernel1Write "gridDetiling" (uvgMarginRepr gcfp ydom0 xdom0) (uvgRepr ydom1 xdom1) kern_detile
foreign import ccall unsafe kern_detile :: HalideFun '[UVGMarginRepr] UVGRepr
