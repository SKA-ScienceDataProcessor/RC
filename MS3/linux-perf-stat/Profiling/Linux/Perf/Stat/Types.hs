
module Profiling.Linux.Perf.Stat.Types where

import Data.Word
import Data.Bits

-- Replicating linux/perf_event.h

-- | Overall event type.
data PerfTypeId
    -- | This indicates one of the "generalized" hardware events
    -- provided by the kernel. See @:PerfHwId@ for more details.
    = PERF_TYPE_HARDWARE   PerfHwId
    -- | This indicates one of the software-defined events provided by
    -- the kernel (even if no hardware support is available).
    | PERF_TYPE_SOFTWARE   PerfSwIds
    -- | This indicates a tracepoint provided by the kernel tracepoint
    -- infrastructure.
    | PERF_TYPE_TRACEPOINT !Word64
    -- | This indicates a hardware cache event.
    | PERF_TYPE_HW_CACHE   PerfHwCacheId PerfHwCacheOpId PerfHwCacheOpResultId
    -- | This indicates a "raw" implementation-specific event in the
    -- config field.
    | PERF_TYPE_RAW        !Word64
    | PERF_TYPE_BREAKPOINT !Word64
    deriving (Eq, Show)

-- | Generalized performance event event_id types
data PerfHwId
    -- | Total cycles. Be wary of what happens during CPU frequency scaling.
    = PERF_COUNT_HW_CPU_CYCLES
    -- | Retired instructions. Be careful, these can be affected by
    -- various issues, most notably hardware interrupt counts.
    | PERF_COUNT_HW_INSTRUCTIONS
    -- | Cache accesses. Usually this indicates Last Level Cache
    -- accesses but this may vary depending on your CPU. This may
    -- include prefetches and coherency messages; again this depends
    -- on the design of your CPU.
    | PERF_COUNT_HW_CACHE_REFERENCES
    -- | Cache misses. Usually this indicates Last Level Cache misses;
    -- this is intended to be used in conjunction with the
    -- PERF_COUNT_HW_CACHE_REFERENCES event to calculate cache miss
    -- rates.
    | PERF_COUNT_HW_CACHE_MISSES
    -- | Retired branch instructions. Prior to Linux 2.6.34, this used
    -- the wrong event on AMD processors.
    | PERF_COUNT_HW_BRANCH_INSTRUCTIONS
    -- | Mispredicted branch instructions.
    | PERF_COUNT_HW_BRANCH_MISSES
    -- | Bus cycles, which can be different from total cycles.
    | PERF_COUNT_HW_BUS_CYCLES
    -- | Stalled cycles during issue. (since Linux 3.0)
    | PERF_COUNT_HW_STALLED_CYCLES_FRONTEND
    -- | Stalled cycles during retirement. (since Linux 3.0)
    | PERF_COUNT_HW_STALLED_CYCLES_BACKEND
    -- | Total cycles; not affected by CPU frequency scaling. (since Linux 3.3)
    | PERF_COUNT_HW_REF_CPU_CYCLES
    deriving (Eq, Enum, Show)

-- | Generalized hardware cache events
data PerfHwCacheId
    = PERF_COUNT_HW_CACHE_L1D   -- ^ for measuring Level 1 Data Cache
    | PERF_COUNT_HW_CACHE_L1I   -- ^ for measuring Level 1 Instruction Cache
    | PERF_COUNT_HW_CACHE_LL    -- ^ for measuring Last-Level Cache
    | PERF_COUNT_HW_CACHE_DTLB  -- ^ for measuring the Data TLB
    | PERF_COUNT_HW_CACHE_ITLB  -- ^ for measuring the Instruction TLB
    | PERF_COUNT_HW_CACHE_BPU   -- ^ for measuring the branch prediction unit
    | PERF_COUNT_HW_CACHE_NODE  -- ^ for measuring local memory accesses
    deriving (Eq, Enum, Show)

data PerfHwCacheOpId
    = PERF_COUNT_HW_CACHE_OP_READ    -- ^ for read accesses
    | PERF_COUNT_HW_CACHE_OP_WRITE   -- ^ for write accesses
    | PERF_COUNT_HW_CACHE_OP_PREFETCH -- ^ for prefetch accesses
      deriving (Eq, Enum, Show)

data PerfHwCacheOpResultId
    = PERF_COUNT_HW_CACHE_RESULT_ACCESS -- ^ to measure accesses
    | PERF_COUNT_HW_CACHE_RESULT_MISS   -- ^ to measure misses
      deriving (Eq, Enum, Show)

-- | Measuring software events provided by the kernel
data PerfSwIds
    -- | This reports the CPU clock, a high-resolution per-CPU timer.
    = PERF_COUNT_SW_CPU_CLOCK
    -- | This reports a clock count specific to the task that is running.
    | PERF_COUNT_SW_TASK_CLOCK
    -- | This reports the number of page faults.
    | PERF_COUNT_SW_PAGE_FAULTS
    -- | This counts context switches. Until Linux 2.6.34, these were
    -- all reported as user-space events, after that they are reported
    -- as happening in the kernel.
    | PERF_COUNT_SW_CONTEXT_SWITCHES
    -- | This reports the number of times the process has migrated to a new CPU.
    | PERF_COUNT_SW_CPU_MIGRATIONS
    -- | This counts the number of minor page faults. These did not
    -- require disk I/O to handle.
    | PERF_COUNT_SW_PAGE_FAULTS_MIN
    -- | This counts the number of major page faults. These required
    -- disk I/O to handle.
    | PERF_COUNT_SW_PAGE_FAULTS_MAJ
    -- | This counts the number of alignment faults. These happen when
    -- unaligned memory accesses happen; the kernel can handle these
    -- but it reduces performance. This happens only on some
    -- architectures (never on x86).
    | PERF_COUNT_SW_ALIGNMENT_FAULTS
    -- | This counts the number of emulation faults. The kernel
    -- sometimes traps on unimplemented instructions and emulates them
    -- for user space. This can negatively impact performance.
    | PERF_COUNT_SW_EMULATION_FAULTS
    -- | This is a placeholder event that counts
    -- nothing. Informational sample record types such as mmap or comm
    -- must be associated with an active event. This dummy event
    -- allows gathering such records without requiring a counting
    -- event.
    | PERF_COUNT_SW_DUMMY
    deriving (Eq, Enum, Show)

-- | Get perf_event event type code
perfTypeCode :: PerfTypeId -> (Word32, Word64)
perfTypeCode (PERF_TYPE_HARDWARE   hwId) = (0, fromIntegral $ fromEnum hwId)
perfTypeCode (PERF_TYPE_SOFTWARE   swId) = (1, fromIntegral $ fromEnum swId)
perfTypeCode (PERF_TYPE_TRACEPOINT swId) = (2, fromIntegral $ fromEnum swId)
perfTypeCode (PERF_TYPE_HW_CACHE hwCacheId hwCacheOpId hwCacheOpResultId)
    = (3, (fromIntegral (fromEnum hwCacheId))
            .|. (fromIntegral (fromEnum hwCacheOpId) `shiftL` 8)
            .|. (fromIntegral (fromEnum hwCacheOpResultId) `shiftL` 16))
perfTypeCode (PERF_TYPE_RAW        rwId) = (4, rwId)
perfTypeCode (PERF_TYPE_BREAKPOINT rwId) = (5, rwId)
