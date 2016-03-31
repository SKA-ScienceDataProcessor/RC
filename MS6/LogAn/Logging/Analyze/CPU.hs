{-# LANGUAGE CPP #-}

module Logging.Analyze.CPU where

import Data.List

import Logging.ProcessLog

data CPUMtxs = CPUMtxs {
    -- Hints
    cmHintFloatOps        :: !Int
  , cmHintDoubleOps       :: !Int
  , cmHintMemoryReadBytes :: !Int
    -- Perf
  , cmCpuCycles           :: !Int
  , cmCpuInstructions     :: !Int
  , cmX87Ops              :: !Int
  , cmScalarFloatOps      :: !Int
  , cmScalarDoubleOps     :: !Int
  , cmSseFloatOps         :: !Int
  , cmSseDoubleOps        :: !Int
  , cmAvxFloatOps         :: !Int
  , cmAvxDoubleOps        :: !Int
  , cmMemReadBytes        :: !Int
    --
  , cmTime                :: !Int
  } deriving Show

zcm :: CPUMtxs
zcm = CPUMtxs 0 0 0 0 0 0 0 0 0 0 0 0 0 0

uch :: CPUMtxs -> ProfileHint -> CPUMtxs
uch mtx (FloatHint fops dops) =
  mtx {
      cmHintFloatOps  = fops
    , cmHintDoubleOps = dops
  }
uch mtx (MemHint mrbs) = mtx {
    cmHintMemoryReadBytes = mrbs
  }
uch mtx _ = mtx

uca :: CPUMtxs -> Attr -> CPUMtxs
uca mtx (Perf ct n t) =
  let mtx1 =
       case ct of
#define _U(f) f -> mtx{cm/**/f = (cm/**/f mtx) + n}
         _U(CpuCycles      )
         _U(CpuInstructions)
         _U(X87Ops         )
         _U(ScalarFloatOps )
         _U(ScalarDoubleOps)
         _U(SseFloatOps    )
         _U(SseDoubleOps   )
         _U(AvxFloatOps    )
         _U(AvxDoubleOps   )
         _U(MemReadBytes   )
#undef _U
  in mtx1{cmTime=cmTime mtx1 + t}
uca mtx _ = mtx

cpuAn :: KernInvDescr -> CPUMtxs
cpuAn kid =
    flip (foldl' uch) (kidHints kid)
  . flip (foldl' uca) (kidAttrs kid)
  $ zcm
