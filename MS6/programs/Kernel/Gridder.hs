{-# LANGUAGE DataKinds, CPP #-}

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
           -> GUVDom               -- ^ GCF u/v domains
           -> UVDom                -- ^ u/v grid domains
           -> Flow Vis -> Flow GCFs -> Flow UVGrid
           -> Kernel UVGrid
gridKernel ktype gp gcfp uvdom wdom guvdom uvdom' =
  hintsByPars (gridHint gcfp ktype) $
  halideKernel2Write (show ktype) (visRepr uvdom wdom)
                                  (gcfsRepr gcfp wdom guvdom)
                                  (uvgMarginRepr gcfp uvdom') $
  gridCKernel ktype `halideBind` gridScale gp
                    `halideBind` fromIntegral (gridHeight gp)
                    `halideBind` fromIntegral (gcfMaxSize gcfp)

gridHint :: GCFPar -> GridKernelType -> [[RegionBox]] -> [ProfileHint]
gridHint gcfp ktype (visRegs:_) = case ktype of
  GridKernelCPU -> [floatHint { hintDoubleOps = ops }, memHint]
#ifdef USE_CUDA
  GridKernelGPU -> [cudaHint { hintCudaDoubleOps = ops } ]
  GridKernelNV  -> [cudaHint { hintCudaDoubleOps = ops } ]
#endif
 where wBinReg = (!!2) -- u, v, w - we want region three (see visRepr definition)
       ops = sum $ map binOps $ concatMap (regionBins . wBinReg) visRegs
       binOps bin = 8 * gcfSize gcf * gcfSize gcf * regionBinSize bin
         where gcf = gcfGet gcfp (regionBinLow bin) (regionBinHigh bin)
gridHint _ _ _ = error "gridHint: Not enough parameters!"

gridCKernel :: GridKernelType -> ForeignGridder
gridCKernel GridKernelCPU = kern_scatter
#ifdef USE_CUDA
gridCKernel GridKernelGPU = kern_scatter_gpu1
gridCKernel GridKernelNV  = nvGridder
#endif

type ForeignGridder = HalideBind Double (HalideBind Int32 (HalideBind Int32 (
                      HalideFun '[VisRepr, GCFsRepr] UVGMarginRepr)))
foreign import ccall unsafe kern_scatter      :: ForeignGridder
#ifdef USE_CUDA
foreign import ccall unsafe kern_scatter_gpu1 :: ForeignGridder
foreign import ccall unsafe nvGridder         :: ForeignGridder
#endif

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
