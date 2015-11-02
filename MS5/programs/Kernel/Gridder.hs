{-# LANGUAGE DataKinds #-}

module Kernel.Gridder where

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
gridInit gp = timeKernel "initKernel" (halideKernel0 "gridInit" (uvgRepr gp) kern_init)
foreign import ccall unsafe kern_init :: HalideFun '[] UVGRepr

-- | Gridder kernel binding
gridKernel :: GridPar -> GCFPar -> DomainHandle Range
           -> Flow Vis -> Flow GCFs -> Flow UVGrid
           -> Kernel UVGrid
-- TODO: Right now the grid gets copied in, but we would actually like
-- it to be updated!
gridKernel gp gcfp dh fv fg fu =
  let k =
         halideKernel2Write "gridKernel" (visRepr dh) (gcfsRepr gcfp) (uvgRepr gp) $
         halideBind kern_scatter (gridTheta gp)
  in timeKernel "gridKernel" (k fv fg fu)

foreign import ccall unsafe kern_scatter
  :: HalideBind Double (HalideFun '[VisRepr, GCFsRepr] UVGRepr)
