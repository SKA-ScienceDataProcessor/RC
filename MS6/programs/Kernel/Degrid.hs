{-# LANGUAGE DataKinds #-}

module Kernel.Degrid where

import Data.Int

import Flow.Builder
import Flow.Halide
import Flow.Kernel

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

-- | Degridder kernel binding
degridKernel :: GridPar -> GCFPar    -- ^ Configuration
             -> DDom  -> DDom        -- ^ Grid and work dataset domains
             -> UVDom -> WDom        -- ^ u/v/w visibility domains
             -> UVDom                -- ^ u/v grid domain
             -> Flow GCFs -> Flow UVGrid -> Flow Vis
             -> Kernel Vis
degridKernel gp gcfp ddom0 ddom1 uvdom wdom uvdom_grid =
  halideKernel3 "degridKernel" (RegionRepr ddom1 $ gcfsRepr wdom gcfp)
                               (RegionRepr ddom0 $ uvgRepr uvdom_grid)
                               (RegionRepr ddom1 $ visRepr uvdom wdom)
                               (RegionRepr ddom1 $ visRepr uvdom wdom) $
  kern_degrid `halideBind` gridScale gp
              `halideBind` fromIntegral (gridHeight gp)
foreign import ccall unsafe kern_degrid
  :: HalideBind Double (HalideBind Int32 (HalideFun '[GCFsRepr, UVGRepr, VisRepr] VisRepr))

