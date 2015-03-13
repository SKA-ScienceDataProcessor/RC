
{-# LANGUAGE CPP #-}

module Profiling.Linux.Perf.Stat
  ( module Profiling.Linux.Perf.Stat.Types

  , perfEventOpen
  , perfEventRead
  , perfEventClose
  , perfEventDiff
  , PerfStatDesc(..)
  , PerfStatGroup
  , PerfStat(..)
  ) where

import Profiling.Linux.Perf.Stat.Types

import Control.Monad          ( forM_, zipWithM, when )

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

foreign import ccall unsafe "perf.h perf_event_open"
   c_perf_event_open :: Word32 -> Word64 -> CInt -> IO CInt

#ifdef USE_LIBPFM
foreign import ccall unsafe "perf.h perf_event_open_pfm"
   c_perf_event_open_pfm :: CString -> CInt -> IO CInt
foreign import ccall unsafe "perfmon/err.h pfm_strerror"
   c_pfm_strerror :: CInt -> IO CString
#endif

-- | Performance counter selector
data PerfStatDesc = PerfDesc PerfTypeId -- ^ Standard @perf_event@ event types
#ifdef USE_LIBPFM
                  | PfmDesc String      -- ^ Use @libpfm@ event description
#endif
                  deriving (Eq, Show)

-- | Open a single performance counter. This is purely internal, as
-- counters should be opened together in groups.
perfEventOpenSingle :: PerfStatDesc -> Maybe CInt -> IO CInt
perfEventOpenSingle (PerfDesc perfTypeId) groupId = do
  let (perfType, perfConfig) = perfTypeCode perfTypeId
  throwErrnoIfMinus1 "perfEventOpen" $
    c_perf_event_open perfType perfConfig (fromMaybe (-1) groupId)
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

newtype PerfStatGroup
    = PerfStatGroup { psCounters :: [(PerfStatDesc, CInt)] }

perfEventOpen :: [PerfStatDesc] -> IO PerfStatGroup
perfEventOpen []      = return $ PerfStatGroup []
perfEventOpen typeIds = do

  -- Open group leader
  groupFd <- perfEventOpenSingle (head typeIds) Nothing

  -- Open other counters
  fds <- mapM (flip perfEventOpenSingle (Just groupFd)) (tail typeIds)
  return $ PerfStatGroup $ zip typeIds (groupFd:fds)

data PerfStat
    = PerfStat { psDesc        :: PerfStatDesc
               , psValue       :: !Word64
               , psTimeEnabled :: !Word64
               , psTimeRunning :: !Word64
               }

perfEventRead :: PerfStatGroup -> IO [PerfStat]
perfEventRead (PerfStatGroup []) = return []
perfEventRead group = do

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
      return $ PerfStat { psDesc = desc
                        , psValue = val
                        , psTimeEnabled = timeEnabled
                        , psTimeRunning = timeRunning
                        }

perfEventClose :: PerfStatGroup -> IO ()
perfEventClose group =
  forM_ (reverse $ psCounters group) $ \(_, fd) ->
    throwErrnoIfMinus1 "perfEventClose" $
      c_close fd

perfEventDiff :: PerfStat -> PerfStat -> PerfStat
perfEventDiff ps1 ps2
  = ps1 { psValue       = psValue ps1 - psValue ps2
        , psTimeEnabled = psTimeEnabled ps1 - psTimeEnabled ps2
        , psTimeRunning = psTimeRunning ps1 - psTimeRunning ps2
        }
