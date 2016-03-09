{-# LANGUAGE DataKinds, CPP #-}

module Kernel.Degrid
  ( distributeGrid, degridKernel
  )
  where

import Data.Int
import qualified Data.Map as Map

import Flow.Builder
import Flow.Halide
import Flow.Kernel
import Flow.Domain
import Flow

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

-- | Trivial kernel for distributing the grid. This is simply about
-- returning the grid unchanged. *Seriously* something dna-flow should
-- learn to to automatically at some point.
distributeGrid :: DDom -> DDom -> LMDom -> GridPar -> Flow FullUVGrid -> Kernel FullUVGrid
distributeGrid ddom0 ddom1 (ldom, mdom) gp =
  mappingKernel "distribute grid" ((RegionRepr ddom0 $ halrWrite $ fullUVGRepr gp) :. Z)
                                  (RegionRepr ddom1 $ RegionRepr ldom $ RegionRepr mdom $ fullUVGRepr gp) $
    \[uvg] _ -> return $ head $ Map.elems uvg


degridKernel :: DegridKernelType
             -> GridPar -> GCFPar    -- ^ Configuration
             -> UVDom -> WDom        -- ^ u/v/w visibility domains
             -> Flow GCFs -> Flow FullUVGrid -> Flow Vis
             -> Kernel Vis
degridKernel ktype gp gcfp uvdom wdom =
  hintsByPars (degridHint gcfp ktype) $
  halideKernel3 (show ktype) (gcfsRepr wdom gcfp)
                             (fullUVGRepr gp)
                             (visRepr uvdom wdom)
                             (visRepr uvdom wdom) $
  foreignDegridder ktype `halideBind` gridScale gp
                         `halideBind` fromIntegral (gridHeight gp)

degridHint :: GCFPar -> DegridKernelType -> [[RegionBox]] -> [ProfileHint]
degridHint gcfp ktype (_:_:visRegs:_) = case ktype of
  DegridKernelCPU -> [floatHint { hintDoubleOps = ops }, memHint]
#ifdef USE_CUDA
  DegridKernelGPU -> [cudaHint  { hintCudaDoubleOps = ops }]
#endif
 where wBinReg = (!!2) -- u, v, w - we want region three (see visRepr definition)
       vis = sum $ map regionBinSize $ concatMap (regionBins . wBinReg) visRegs
       ops = 8 * gcfSize gcfp * gcfSize gcfp * vis
degridHint _ _ _ = error "degridHint: Not enough parameters!"

type ForeignDegridder = HalideBind Double (HalideBind Int32 (
                                              HalideFun '[GCFsRepr, FullUVGRepr, VisRepr] VisRepr))
foreignDegridder :: DegridKernelType -> ForeignDegridder
foreignDegridder DegridKernelCPU = kern_degrid
#ifdef USE_CUDA
foreignDegridder DegridKernelGPU = kern_degrid_gpu1
#endif
foreign import ccall unsafe kern_degrid      :: ForeignDegridder
#ifdef USE_CUDA
foreign import ccall unsafe kern_degrid_gpu1 :: ForeignDegridder
#endif
