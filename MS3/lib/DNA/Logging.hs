{-# LANGUAGE BangPatterns #-}
-- | Logging.hs
--
-- Logging and profiling facilities. Log messages are written to GHC's
-- eventlog in the following format:
--
-- > TAG [ATTR]* message
--
-- The tag is a sequence of alphanumeric characters, usually in all
-- caps. The tag can be:
--
--  *  MSG: for simple log messages
--  *  FATAL: log messages reporting errors
--  *  SYNC: for synchronising time between nodes
--  *  START x/END x: sample of a performance metric (for profiling)
--
-- Attributes can be used to add (possibly optional) extra data to the
-- message. They are enclosed in square brackets and must precede the
-- message. Possible attributes are 
--
--  * [pid=PID]: for the Cloud Haskell process ID. For messages
--    concerning the whole program, this may not be set.
--
--  * [SAMPLER:METRIC=VAL/TIME]: records the value of a performance
--    metric. When a time is given, it is the time that the counter
--    has been running.
--
--  * [hint:METRIC=VAL]: A performance hint by the program, such as
--    expected number of floating point operations.
--
-- Copyright (C) 2014-2015 Braam Research, LLC.
module DNA.Logging
    ( taggedMessage
    , eventMessage
    , synchronizationPoint
    , logDuration
    , logProfile

    , ProfileHint(..)
    ) where

import Control.Applicative
import Control.Distributed.Process (getSelfPid)
import Control.Exception           (evaluate)
import Control.Monad               (liftM)
import Control.Monad.IO.Class

import Data.Time
import Data.Maybe         (fromMaybe)

import GHC.Stats

import Debug.Trace        (traceEventIO)

import Profiling.Linux.Perf.Stat

import System.IO.Unsafe   (unsafePerformIO)
import System.Locale      (defaultTimeLocale)
import System.Mem         (performGC)

import DNA.Types

----------------------------------------------------------------
-- Message data types for logger
----------------------------------------------------------------

type Attr = (String, String)

-- | Generate the specified eventlog message
message :: MonadIO m
        => String -- ^ Message tag
        -> [Attr] -- ^ Message attributes
        -> String -- ^ Message body
        -> m ()
message tag attrs msg = do
    let formatAttr (attr, val) = ' ':'[':attr ++ '=': val ++ "]"
    liftIO $ traceEventIO $ concat (tag : map formatAttr attrs) ++ ' ':msg

-- | Output a custom-tag process message into the eventlog.
taggedMessage :: MonadProcess m
              => String         -- ^ Message tag
              -> String         -- ^ Message
              -> m ()
taggedMessage tag msg = do
    pid <- liftP getSelfPid
    message tag [("pid", show pid)] msg

-- | Put a global message into eventlog.
eventMessage :: MonadIO m => String -> m ()
eventMessage = message "MSG" []

-- | Synchronize timings - put into eventlog an event with current wall time.
synchronizationPoint :: MonadIO m => String -> m ()
synchronizationPoint msg = liftIO $ do
    utcTime <- getCurrentTime
    -- we are formatting time to number of seconds in POSIX epoch and
    -- fractional part in picoseconds.
    let timeString    = formatTime defaultTimeLocale "%s.%q" utcTime
        humanReadable = formatTime defaultTimeLocale "%F %X" utcTime
    message "SYNC" [("time", timeString)] msg
    message "MSG" [] $ "started at " ++ humanReadable

----------------------------------------------------------------
-- Profiling basics
----------------------------------------------------------------

-- | Put measurements about execution time of monadic action into
--   eventlog. Result of action is evaluated to WHNF.
measurement :: MonadIO m
               => m [Attr] -- ^ Measurements, might add extra attributes
               -> [Attr]   -- ^ Attributes
               -> String   -- ^ Message
               -> m a      -- ^ DNA action to profile
               -> m a
measurement sample attrs msg dna = do
    -- Get start sample
    sample0 <- sample
    message "START" sample0 msg
    -- Perform action
    r <- liftIO . evaluate =<< dna
    -- Get end sample, return
    sample1 <- sample
    message "END" (attrs ++ sample1) msg
    return r

-- | Same as @measurement@, but automatically adds process attributes
procMeasurement :: MonadProcess m
               => m [Attr] -- ^ Measurements, might add extra attributes
               -> [Attr]   -- ^ Attributes
               -> String   -- ^ Message
               -> m a      -- ^ DNA action to profile
               -> m a
procMeasurement sample attrs msg dna = do
    pid <- liftP getSelfPid
    measurement sample (("pid", show pid):attrs) msg dna

-- | Put measurements about execution time of monadic action into
--   eventlog. Result of action is evaluated to WHNF.
logDuration :: MonadProcess m => String -> m a -> m a
logDuration = procMeasurement (return []) []
               -- measurement is implicit from START/END timestamp

{- PMW: unused?

-- | Measure time period of pure computation into eventlog.  It's
--   strict in argument. Because action is pure we put empty PID into
--   eventlog.
timePeriodPure :: String -> a -> a
timePeriodPure ev a = unsafeDupablePerformIO $ do
    traceEventIO ("START [] "++ev)
    a `seq` traceEventIO ("END [] "++ev)
    return a
-}

----------------------------------------------------------------
-- Profiling
----------------------------------------------------------------

-- | A program annotation providing additional information about how
-- much work we expect the program to be doing in a certain phase. The
-- purpose of this hint is that we can set-up measurements to match
-- these numbers to the program's real performance.
--
-- Note that this is just a hint - a best effort should be made to
-- give a realistic estimate. As a rule of thumb, it is better to use
-- a more conservative estimate, as this will generally result in
-- lower performance estimates (in profiling, false positives are
-- better than false negatives).
data ProfileHint
    = FloatHint { hintFloatOps :: Int -- ^ Number of single-precision operations
                , hintDoubleOps :: Int -- ^ Number of double-precision operations
                }
      -- ^ Estimate for how much floating point operations the code is doing
    | IOHint { hintReadBytes :: Int
             , hintWriteBytes :: Int
             }
      -- ^ Estimate for how much data the program is reading or
      -- writing from/to external sources.
    | HaskellHint { hintAllocation :: Int
                  }
      -- ^ Rough estimate for how much Haskell work we are doing.
    | CUDAHint { cudaReadBytes :: Int
               , cudaWriteBytes :: Int
               , cudaOps :: Int
               }
      -- ^ Just a stub for now, need to figure out how to make
      -- measurements happen for this.

-- | Main profiling function. The concrete information output to the
-- event log depends on the hints about the code's actions.
--
-- Generally, the more hints we have about the code's actions, the
-- better. However, also note that more hints generally means that we
-- are going to safe more information, so keep in mind that every hint
-- means a certain (constant) profiling overhead.
logProfile :: MonadProcess m
        => String           -- ^ Message. Will be used in profile view
                            -- to identify costs, so short and
                            -- recognisable names are preferred.
        -> [ProfileHint]    -- ^ Hints about the code's complexity.
        -> m a              -- ^ The code to profile
        -> m a
logProfile msg hints = procMeasurement (liftIO sample) [] msg
    where sample = concat `liftM` mapM hintToSample hints

-- | Takes a sample according to the given hint
hintToSample :: ProfileHint -> IO [Attr]
hintToSample fh@FloatHint{}
    = consAttrNZ "hint:float-ops" (hintFloatOps fh)
    . consAttrNZ "hint:double-ops" (hintDoubleOps fh)
    <$> floatCounterAttrs
hintToSample ioh@IOHint{}
    = consAttrNZ "hint:read-bytes" (hintReadBytes ioh)
    . consAttrNZ "hint:write-bytes" (hintWriteBytes ioh)
    <$> ioAttrs
hintToSample hh@HaskellHint{}
    = consAttrNZ "hint:haskell-alloc" (hintAllocation hh)
    <$> haskellAttrs
hintToSample CUDAHint{}
    = return []

-- | Prepend an attribute if it is non-zero
consAttrNZ :: (Eq a, Num a, Show a)
           => String -> a -> [Attr] -> [Attr]
consAttrNZ _ 0 = id
consAttrNZ n v = ((n, show v):)

----------------------------------------------------------------
-- perf_events sampling
----------------------------------------------------------------

-- | The floating point counters, with associated names
floatCounterDescs :: [(String, PerfStatDesc)]
floatCounterDescs
  = [ ("cpu-cycles",        PerfDesc $ PERF_TYPE_HARDWARE PERF_COUNT_HW_CPU_CYCLES)
    , ("cpu-instructions",  PerfDesc $ PERF_TYPE_HARDWARE PERF_COUNT_HW_INSTRUCTIONS)
    , ("x87-ops",           PfmDesc "FP_COMP_OPS_EXE:X87")
    , ("scalar-float-ops",  PfmDesc "FP_COMP_OPS_EXE:SSE_FP_SCALAR_SINGLE")
    , ("scalar-double-ops", PfmDesc "FP_COMP_OPS_EXE:SSE_SCALAR_DOUBLE")
    , ("sse-float-ops",     PfmDesc "FP_COMP_OPS_EXE:SSE_PACKED_SINGLE")
    , ("sse-double-ops",    PfmDesc "FP_COMP_OPS_EXE:SSE_FP_PACKED_DOUBLE")
    , ("avx-float-ops",     PfmDesc "SIMD_FP_256:PACKED_SINGLE")
    , ("avx-float-ops",     PfmDesc "SIMD_FP_256:PACKED_DOUBLE")
    ]

-- | Floating point perf_event counters. This is a lazy global
-- constant at the moment - meaning that the counters will get
-- allocated on the first use of this variable, and will stay open
-- until the process finishes.
floatCounters :: PerfStatGroup
floatCounters = unsafePerformIO $ perfEventOpen $ map snd floatCounterDescs

-- | Generate message attributes from current floating point counter values
floatCounterAttrs :: IO [Attr]
floatCounterAttrs = do
    -- Get counters from perf_event
    vals <- perfEventRead floatCounters
    -- Generate attributes
    let fmtName (name, _) = "perf:" ++ name
        fmtVal stat = show (psValue stat) ++ "/" ++ show (psTimeRunning stat)
    return $ zip (map fmtName floatCounterDescs) (map fmtVal vals)

----------------------------------------------------------------
-- I/O data sampling
----------------------------------------------------------------

-- | Generate message attributes for procces I/O statistics
ioAttrs :: IO [Attr]
ioAttrs = do

  -- Read /proc/self/io - not the full story by any means, especially
  -- when consindering mmap I/O (TODO!), but it's easy.
  ios <- map (break (==':')) . lines <$> readFile "/proc/self/io"
  let io name = drop 2 $ fromMaybe "" $ lookup name ios
  return [ ("proc:read-bytes", io "read_bytes")
         , ("proc:write-bytes", io "write_bytes")
         ]

----------------------------------------------------------------
-- Haskell RTS sampling
----------------------------------------------------------------

-- | Generate message attributes for procces I/O statistics
haskellAttrs :: IO [Attr]
haskellAttrs = do

    -- This might be slightly controversial: This forces a GC so we get
    -- statistics about the *true* memory residency.
    performGC

    -- Now get statistics
    available <- getGCStatsEnabled
    if not available then return [] else do
        stats <- getGCStats
        return [ ("rts:haskell-alloc",   show $ bytesAllocated stats)
               , ("rts:gc-bytes-copied", show $ bytesCopied stats)
               , ("rts:mut-time",        show $ mutatorCpuSeconds stats)
               , ("rts:gc-time",         show $ gcCpuSeconds stats)
               , ("rts:heap-size",       show $ currentBytesUsed stats)
               ]

