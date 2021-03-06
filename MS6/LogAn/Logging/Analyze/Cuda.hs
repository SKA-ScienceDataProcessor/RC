{-# LANGUAGE CPP #-}

module Logging.Analyze.Cuda where

import Data.List

import Logging.ProcessLog

data GpuMtxs = GpuMtxs {
    -- Hints
    gmHintCopyBytesHost   :: !Int
  , gmHintCopyBytesDevice :: !Int
  , gmHintCudaFloatOps    :: !Int
  , gmHintCudaDoubleOps   :: !Int
    --
  , gmGpuDoubleOps        :: !Int
  , gmGpuDoubleOpsAdd     :: !Int
  , gmGpuDoubleOpsMul     :: !Int
  , gmGpuDoubleOpsFMA     :: !Int
    -- paired
  , gmMemsetBytes  :: !Int , gmMemsetBytesTime  :: !Int
  , gmMemcpyBytesH :: !Int , gmMemcpyBytesHTime :: !Int
  , gmMemcpyBytesD :: !Int , gmMemcpyBytesDTime :: !Int
    --
  , gmKernelTime   :: !Int
  , gmOverheadTime :: !Int
  } deriving Show

zgm :: GpuMtxs
zgm = GpuMtxs 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

uh :: GpuMtxs -> ProfileHint -> GpuMtxs
uh mtx (CUDAHint cbh cbd fops dops) =
  mtx {
      gmHintCopyBytesHost   = cbh
    , gmHintCopyBytesDevice = cbd
    , gmHintCudaFloatOps    = fops
    , gmHintCudaDoubleOps   = dops
  }
uh mtx _ = mtx

ua :: GpuMtxs -> Attr -> GpuMtxs
ua mtx (CO co n _) =
  case co of
#define _U(f) f -> mtx{gm/**/f = (gm/**/f mtx) + n}
    _U(GpuDoubleOps)
    _U(GpuDoubleOpsAdd)
    _U(GpuDoubleOpsMul)
    _U(GpuDoubleOpsFMA)
#undef _U
ua mtx _ = mtx

up :: GpuMtxs -> (Attr, Attr) -> GpuMtxs
up mtx (CM cm1 n1 t1, CM cm2 n2 t2) = check cm1 cm2 $
#define _UT(f) f -> mtx{gm/**/f = (gm/**/f mtx) + n2 - n1, gm/**/f/**/Time = (gm/**/f/**/Time mtx) + t2 - t1}
  case cm1 of
    _UT(MemsetBytes)
    _UT(MemcpyBytesH)
    _UT(MemcpyBytesD)
#undef _UT
up mtx (CT ct1 n1, CT ct2 n2) = check ct1 ct2 $
#define _U(f) f -> mtx{gm/**/f = (gm/**/f mtx) + n2 - n1}
  case ct1 of
    _U(KernelTime)
    _U(OverheadTime)
#undef _U
up _ (c1, c2) = error $ "Internal error in up: " ++ show c1 ++ '/':show c2

cudaAn :: KernInvDescr -> GpuMtxs
cudaAn kid =
    flip (foldl' uh) (kidHints kid)
  . flip (foldl' ua) (kidAttrs kid)
  . flip (foldl' up) (kidAPairs kid)
  $ zgm
