
module Kernel.Gridder where

import Flow.Builder
import Flow.Domain
import Flow.Kernel

import Kernel.Data

gridInit :: GridPar -> Kernel UVGrid
gridInit gp = halideKernel0 "gridInit" (uvgRepr gp) undefined

gridKernel :: GridPar -> GCFPar -> DomainHandle Range
           -> Flow Vis -> Flow GCFs -> Flow UVGrid
           -> Kernel UVGrid
gridKernel gp gcfp dh = halideKernel3 "gridKernel" (visRepr dh) (gcfsRepr gcfp) (uvgRepr gp) (uvgRepr gp) undefined

