{-# LANGUAGE DataKinds #-}

module Kernel.Gridder
  ( GridKernelType
  , gridInit, gridKernel
  , gridInitDetile, gridDetiling
  )
  where

import Data.Int

import Flow.Builder
import Flow.Halide
import Flow.Domain
import Flow

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

-- | Gridder grid initialisation. This is separate because
-- "gridKernel" requires an input to work.
gridInit :: GCFPar -> UVDom -> Kernel UVGrid
gridInit gcfp uvdom = halideKernel0 "gridInit" (uvgMarginRepr gcfp uvdom) kern_init
foreign import ccall unsafe kern_init :: HalideFun '[] UVGRepr

-- | Make gridder kernel binding
gridKernel :: GridKernelType -> GridPar -> GCFPar -- ^ Configuration
           -> UVDom -> WDom        -- ^ u/v/w visibility domains
           -> UVDom                -- ^ u/v grid domains
           -> Flow Vis -> Flow GCFs -> Flow UVGrid
           -> Kernel UVGrid
gridKernel ktype gp gcfp uvdom wdom uvdom' =
  hintsByPars (gridHint ktype) $
  halideKernel2Write (show ktype) (visRepr uvdom wdom)
                                  (gcfsRepr wdom gcfp)
                                  (uvgMarginRepr gcfp uvdom') $
  gridCKernel ktype `halideBind` gridScale gp
                    `halideBind` fromIntegral (gridHeight gp)

gridHint :: GridKernelType -> [[RegionBox]] -> [ProfileHint]
gridHint ktype (visRegs:_) = case ktype of
  GridKernelCPU -> [floatHint { hintDoubleOps = ops }, memHint]
  GridKernelGPU -> [cudaHint { hintCudaDoubleOps = ops } ]
  GridKernelNV  -> [cudaHint { hintCudaDoubleOps = ops } ]
 where wBinReg = (!!2) -- u, v, w - we want region three (see visRepr definition)
       ops = sum $ map regionBinSize $ concatMap (regionBins . wBinReg) visRegs
gridHint _ _ = error "gridHint: Not enough parameters!"

gridCKernel :: GridKernelType -> ForeignGridder
gridCKernel GridKernelCPU = kern_scatter
gridCKernel GridKernelGPU = kern_scatter_gpu1
gridCKernel GridKernelNV  = nvGridder

type ForeignGridder = HalideBind Double (HalideBind Int32 (HalideFun '[VisRepr, GCFsRepr] UVGMarginRepr))
foreign import ccall unsafe kern_scatter      :: ForeignGridder
foreign import ccall unsafe kern_scatter_gpu1 :: ForeignGridder
foreign import ccall unsafe nvGridder         :: ForeignGridder

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
