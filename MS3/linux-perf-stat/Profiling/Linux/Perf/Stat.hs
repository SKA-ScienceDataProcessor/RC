
{-# LANGUAGE CPP #-}

module Profiling.Linux.Perf.Stat
  ( module Profiling.Linux.Perf.Stat.Types

  , perfEventOpen
  , perfEventRead
  , perfEventEnable
  , perfEventDisable
  , perfEventClose
  , perfEventDiff
  , PerfStatDesc(..)
  , PerfStatGroup
  , PerfStatCount(..)
  ) where

import Profiling.Linux.Perf.Stat.Types
import Profiling.Linux.Perf.Stat.PMU

import Control.Monad

import Data.Word
import Data.Maybe             ( fromMaybe )

import Foreign.C.Error        ( throwErrnoIfMinus1 )
import Foreign.C.Types
#ifdef USE_LIBPFM
import Foreign.C.String       ( CString, withCString, peekCString )
#endif
import Foreign.Marshal.Alloc  ( allocaBytes )
import Foreign.Ptr            ( Ptr, castPtr )
import Foreign.Storable       ( peekElemOff )

import System.Posix.Internals ( c_close, c_read )

import GHC.IO.Exception       ( IOException(..), IOErrorType(..) )

foreign import ccall unsafe "perf_event_open"
   c_perf_event_open :: Word32 -> Word64 -> Word64 -> Word64 -> CInt -> IO CInt
foreign import ccall unsafe "perf_event_enable"
   c_perf_event_enable :: CInt -> IO CInt
foreign import ccall unsafe "perf_event_disable"
   c_perf_event_disable :: CInt -> IO CInt

#ifdef USE_LIBPFM
foreign import ccall unsafe "perf_event_open_pfm"
   c_perf_event_open_pfm :: CString -> CInt -> IO CInt
foreign import ccall unsafe "pfm_strerror"
   c_pfm_strerror :: CInt -> IO CString
#endif

-- | Performance counter selector
data PerfStatDesc
    = PerfDesc PerfTypeId   -- ^ Standard @perf_event@ event types
    | PMUDesc String String -- ^ Read @/sys/bus/event_source@. First
                            -- parameter is the device, second the
                            -- counter.
    | PfmDesc String        -- ^ Use @libpfm@ event description. If
                            -- @linux-perf-stat@ was compiled without
                            -- @libpfm@ support, some hard-coded counters
                            -- might still work.
    deriving (Eq, Show)

-- | Open a single performance counter. This is purely internal, as
-- counters should be opened together in groups.
perfEventOpenSingle :: PerfStatDesc -> Maybe CInt -> IO CInt
perfEventOpenSingle (PerfDesc perfTypeId) groupId = do
  let (perfType, config, config1, config2) = perfTypeCode perfTypeId
  throwErrnoIfMinus1 "perfEventOpen" $
    c_perf_event_open perfType config config1 config2 (fromMaybe (-1) groupId)
perfEventOpenSingle (PMUDesc deviceName counterName) groupId = do
  (perfType, config, config1, config2) <- pmuTypeCode deviceName counterName
  throwErrnoIfMinus1 "perfEventOpen" $
    c_perf_event_open perfType config config1 config2 (fromMaybe (-1) groupId)
#ifdef USE_LIBPFM
perfEventOpenSingle (PfmDesc pfmDesc) groupId =
  withCString pfmDesc $ \pfmDescC -> do
    ret <- throwErrnoIfMinus1 "perfEventOpen" $
      c_perf_event_open_pfm pfmDescC (fromMaybe (-1) groupId)
    -- Catch PFM errors
    when (ret == -2) $ do
      errStr <- peekCString =<< c_pfm_strerror (fromIntegral ret)
      ioError $ perfEventError $ "libpfm: " ++ errStr
    return ret
#else
perfEventOpenSingle (PfmDesc pfmDesc) groupId = do
  config <- case pfmDesc of
    "FP_COMP_OPS_EXE:X87"                  -> return 0x530110
    "FP_COMP_OPS_EXE:SSE_FP_SCALAR_SINGLE" -> return 0x532010
    "FP_COMP_OPS_EXE:SSE_SCALAR_DOUBLE"    -> return 0x538010
    "FP_COMP_OPS_EXE:SSE_PACKED_SINGLE"    -> return 0x534010
    "FP_COMP_OPS_EXE:SSE_FP_PACKED_DOUBLE" -> return 0x531010
    "SIMD_FP_256:PACKED_SINGLE"            -> return 0x530111
    "SIMD_FP_256:PACKED_DOUBLE"            -> return 0x530211
    _other -> ioError $ perfEventError "Need libpfm support to look up perf_event counter!"
  let desc = PerfDesc $ PERF_TYPE_RAW config 0 0
  perfEventOpenSingle desc groupId
#endif

-- | Constructor for our errors - let's just use @IOException@ for the
-- moment.
perfEventError :: String -> IOException
perfEventError cause = IOError { ioe_handle = Nothing
                               , ioe_type   = IllegalOperation
                               , ioe_location = "perf_event"
                               , ioe_description = cause
                               , ioe_errno  = Nothing
                               , ioe_filename = Nothing
                               }

-- | A group of performance counters
newtype PerfStatGroup
    = PerfStatGroup { psCounters :: [(PerfStatDesc, CInt)] }

-- | Open a group of performance counters. @perf_event@ guarantees
-- that the given counters will all be active at the same time on the
-- same CPU, so they can be meaningfully compared. Note that counters
-- start off disabled, use @perfEventEnable@ to start counting.
perfEventOpen :: [PerfStatDesc] -> IO PerfStatGroup
perfEventOpen []      = return $ PerfStatGroup []
perfEventOpen typeIds = do

  -- Open group leader
  groupFd <- perfEventOpenSingle (head typeIds) Nothing

  -- Open other counters
  fds <- mapM (flip perfEventOpenSingle (Just groupFd)) (tail typeIds)
  return $ PerfStatGroup $ zip typeIds (groupFd:fds)

-- | Performance counter snapshot
data PerfStatCount = PerfStatCount
    { psDesc        :: PerfStatDesc -- ^ Event description
    , psValue       :: !Word64 -- ^ Counter value
    , psTimeEnabled :: !Word64 -- ^ Time this timer has been
                               -- enabled. Will be the same on all
                               -- counters of a group.
    , psTimeRunning :: !Word64 -- ^ Time this timer has been
                               -- running. Can be substantially less
                               -- than @psTimeEnabled@ if the OS ran
                               -- out of counter resources and had to
                               -- multiplex. However, this is still
                               -- guaranteed to be the same on all
                               -- counters of a group.
    }

-- | Read the current performance counter values
perfEventRead :: PerfStatGroup -> IO [PerfStatCount]
perfEventRead (PerfStatGroup []) = return []
perfEventRead group = do

#ifdef USE_PERF_FORMAT_GROUP

  -- Calculate size of one complete sample of all counters plus timers
  let counters = length $ psCounters group
      size     = 8 * (3 + fromIntegral counters)
  allocaBytes (fromIntegral size) $ \buf -> do

    -- Read from our group file descriptor
    -- Note: This will throw a resource-exhausted error if we passed a
    -- buffer that was too small.
    let groupFd = snd (head (psCounters group))
    size' <- throwErrnoIfMinus1 "perfEventRead" $
      c_read groupFd buf size
    when (size /= fromIntegral size') $
      ioError $ perfEventError $ "unexpected counter reading size: " ++
                                 show size ++ " != " ++ show size' ++ "!"

    -- First word is number of counters - do an extra check
    let vals = castPtr buf :: Ptr Word64
    counters' <- peekElemOff vals 0
    when (counters /= fromIntegral counters') $
      ioError $ perfEventError $ "unexpected counter reading number: " ++
                                 show counters ++ " != " ++ show counters' ++ "!"

    -- Read time enabled & time running
    timeEnabled <- peekElemOff vals 1
    timeRunning <- peekElemOff vals 2

    -- Generate stats. Note that we need to "jump" over time
    -- enabled/running, which come after the first counter value but
    -- before all others.
    (\f -> zipWithM f [3..] (psCounters group)) $ \i (desc, _) -> do
      val <- peekElemOff vals i
      return $ PerfStatCount { psDesc = desc
                             , psValue = val
                             , psTimeEnabled = timeEnabled
                             , psTimeRunning = timeRunning
                             }

#else

  -- Calculate size of one sample plus timers
  let size = 8 * 3
  allocaBytes (fromIntegral size) $ \buf ->
    forM (psCounters group) $ \(desc, fd) -> do

      -- Read from counter file descriptor
      size' <- throwErrnoIfMinus1 "perfEventRead" $
        c_read fd buf size
      when (size /= fromIntegral size') $
        ioError $ perfEventError $ "unexpected counter reading size: " ++
                                   show size ++ " != " ++ show size' ++ "!"

      -- Read time enabled & time running
      let vals = castPtr buf :: Ptr Word64
      timeEnabled <- peekElemOff vals 1
      timeRunning <- peekElemOff vals 2

      -- Generate stats.
      val <- peekElemOff vals 0
      return $ PerfStatCount { psDesc = desc
                             , psValue = val
                             , psTimeEnabled = timeEnabled
                             , psTimeRunning = timeRunning
                             }
#endif

-- | Enable performance counters
perfEventEnable :: PerfStatGroup -> IO ()
perfEventEnable group =
  -- FIXME: The documentation assures us that enabling the group
  -- leader is enough to catch all sub-counters. Unfortunately, this
  -- seems to be quite unreliable. So for now, we just enable them all
  -- "manually".
  forM_ (psCounters group) $
    throwErrnoIfMinus1 "perfEventEnable" . c_perf_event_enable . snd

-- | Disable performance counters
perfEventDisable :: PerfStatGroup -> IO ()
perfEventDisable group =
    void $ throwErrnoIfMinus1 "perfEventDisable" $
        c_perf_event_disable $ snd $ head $ psCounters group

-- | Close a group of performance counters
perfEventClose :: PerfStatGroup -> IO ()
perfEventClose group =
  forM_ (reverse $ psCounters group) $ \(_, fd) ->
    throwErrnoIfMinus1 "perfEventClose" $
      c_close fd

-- | Utility function to find difference of counter values
perfEventDiff :: PerfStatCount -> PerfStatCount -> PerfStatCount
perfEventDiff ps1 ps2
  = ps1 { psValue       = psValue ps1 - psValue ps2
        , psTimeEnabled = psTimeEnabled ps1 - psTimeEnabled ps2
        , psTimeRunning = psTimeRunning ps1 - psTimeRunning ps2
        }
