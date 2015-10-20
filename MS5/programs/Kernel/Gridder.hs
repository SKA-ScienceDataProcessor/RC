{-# LANGUAGE DataKinds #-}

module Kernel.Gridder where

import Flow.Builder
import Flow.Domain
import Flow.Halide

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

gridInit :: GridPar -> Kernel UVGrid
gridInit gp = halideKernel0 "gridInit" (uvgRepr gp) undefined

-- | Gridder kernel binding
gridKernel :: GridPar -> GCFPar -> DomainHandle Range
           -> Flow Vis -> Flow GCFs -> Flow UVGrid
           -> Kernel UVGrid
-- TODO: Right now the grid gets copied in, but we would actually like
-- it to be updated!
gridKernel gp gcfp dh
  = halideKernel3 "gridKernel" (visRepr dh) (gcfsRepr gcfp) (uvgRepr gp) (uvgRepr gp) $
    halideBind kern_scatter (gridTheta gp)
foreign import ccall unsafe kern_scatter
  :: HalideBind Double (HalideFun '[VisRepr, GCFsRepr, UVGridRepr] UVGridRepr)
