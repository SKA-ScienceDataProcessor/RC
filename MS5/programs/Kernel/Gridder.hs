{-# LANGUAGE DataKinds #-}

module Kernel.Gridder where

import Data.Int

import Flow.Builder
import Flow.Halide

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

-- | Gridder grid initialisation. This is separate because
-- "gridKernel" requires an input to work.
gridInit :: GCFPar -> UVDom -> Kernel UVGrid
gridInit gcfp uvdom = halideKernel0 "gridInit" (uvgMarginRepr gcfp uvdom) kern_init
foreign import ccall unsafe kern_init :: HalideFun '[] UVGRepr

-- | Gridder kernel binding
gridKernel :: GridPar -> GCFPar    -- ^ Configuration
           -> UVDom -> WDom        -- ^ u/v/w visibility domains
           -> UVDom                -- ^ u/v grid domains
           -> Flow Vis -> Flow GCFs -> Flow UVGrid
           -> Kernel UVGrid
gridKernel gp gcfp uvdom wdom uvdom' =
  halideKernel2Write "gridKernel" (visRepr uvdom wdom)
                                  (gcfsRepr wdom gcfp)
                                  (uvgMarginRepr gcfp uvdom') $
  kern_scatter `halideBind` (gridTheta gp / fromIntegral (gridFacets gp))
               `halideBind` fromIntegral (gridHeight gp)
foreign import ccall unsafe kern_scatter
  :: HalideBind Double (HalideBind Int32 (HalideFun '[VisRepr, GCFsRepr] UVGMarginRepr))

-- | Gridder grid initialisation, for detiling. Only differs from
-- "gridInit" in the produced data representation, we can even re-use
-- the underlying Halide kernel.
gridInitDetile :: UVDom -> Kernel UVGrid
gridInitDetile uvdom = halideKernel0 "gridInitDetile" (uvgRepr uvdom) kern_init

-- | Grid de-tiling kernel. This simply copies tiled (and possibly
-- overlapped) tiles into a common UV-grid.
gridDetiling :: GCFPar -> UVDom -> UVDom
             -> Flow UVGrid -> Flow UVGrid -> Kernel UVGrid
gridDetiling gcfp uvdom0 uvdom1 =
  halideKernel1Write "gridDetiling" (uvgMarginRepr gcfp uvdom0) (uvgRepr uvdom1) kern_detile
foreign import ccall unsafe kern_detile :: HalideFun '[UVGMarginRepr] UVGRepr
