{-# LANGUAGE CPP #-}

module Logging.Analyze.CPU where

import Data.Word
import Data.List

import Logging.ProcessLog

data CPUMtxs = CPUMtxs {
    -- Hints
    cmHintFloatOps        :: !Int
  , cmHintDoubleOps       :: !Int
  , cmHintMemoryReadBytes :: !Int
    -- Perf
  , cmCpuCycles       :: !Int -- , cmCpuCyclesTime       :: !Int
  , cmCpuInstructions :: !Int -- , cmCpuInstructionsTime :: !Int
  , cmX87Ops          :: !Int -- , cmX87OpsTime          :: !Int
  , cmScalarFloatOps  :: !Int -- , cmScalarFloatOpsTime  :: !Int
  , cmScalarDoubleOps :: !Int -- , cmScalarDoubleOpsTime :: !Int
  , cmSseFloatOps     :: !Int -- , cmSseFloatOpsTime     :: !Int
  , cmSseDoubleOps    :: !Int -- , cmSseDoubleOpsTime    :: !Int
  , cmAvxFloatOps     :: !Int -- , cmAvxFloatOpsTime     :: !Int
  , cmAvxDoubleOps    :: !Int -- , cmAvxDoubleOpsTime    :: !Int
  , cmMemReadBytes    :: !Int -- , cmMemReadBytesTime    :: !Int
  -- Wall time
  , cmTime :: !Word64
  } deriving Show

zcm :: CPUMtxs
zcm = CPUMtxs 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -- 0 0 0 0 0 0 0 0 0

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
-- uca mtx (Perf ct n t) =
uca mtx (Perf ct n _) =
  case ct of
-- #define _U(f) f -> mtx{cm/**/f = (cm/**/f mtx) + n, cm/**/f/**/Time = (cm/**/f/**/Time mtx) + t}
#define _U(f) f -> mtx{cm/**/f = (cm/**/f mtx) + n}
    _U(CpuCycles)
    _U(CpuInstructions)
    _U(X87Ops)
    _U(ScalarFloatOps)
    _U(ScalarDoubleOps)
    _U(SseFloatOps)
    _U(SseDoubleOps)
    _U(AvxFloatOps)
    _U(AvxDoubleOps)
    _U(MemReadBytes)
#undef _U
uca mtx _ = mtx

cpuAn :: KernInvDescr -> CPUMtxs
cpuAn kid =
    flip (foldl' uch) (kidHints kid)
  . flip (foldl' uca) (kidAttrs kid)
  $ zcm {cmTime = kidEnd kid - kidStart kid}
