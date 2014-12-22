{-# LANGUAGE BangPatterns #-}
-- | Logging.hs
--
-- Logging facilities.
--
-- Copyright (C) 2014 Braam Research, LLC.
module DNA.Logging where

import Control.Monad.IO.Class
import Control.Distributed.Process (getSelfPid)
import Data.Time
import System.IO.Unsafe   (unsafeDupablePerformIO)
import System.Locale      (defaultTimeLocale)
import Text.Printf        (printf)
import Debug.Trace        (traceEventIO)

import DNA.Types



----------------------------------------------------------------
-- Message data types for logger
----------------------------------------------------------------

-- |Put measurements about time period into eventlog.
timePeriod :: MonadIO m => String -> m a -> m a
timePeriod ev a = do
    liftIO $ traceEventIO $ "START "++ev
    !r <- a
    liftIO $ traceEventIO $ "END "++ev
    return r

-- |Measure time period of pure computation into eventlog.
-- Is strict on argument.
timePeriodPure :: String -> a -> a
timePeriodPure ev a = unsafeDupablePerformIO $ do
    traceEventIO ("START "++ev)
    a `seq` traceEventIO ("END "++ev)
    return a

-- |Synchronize timings - put into eventlog an event with current wall time.
synchronizationPoint :: MonadIO m => String -> m ()
synchronizationPoint msg = liftIO $ do
    utcTime <- getCurrentTime
    -- we are formatting time to number of seconds in POSIX epoch and
    -- fractional part in picoseconds.
    let timeString = formatTime defaultTimeLocale "%s.%q" utcTime
    traceEventIO $ "SYNC " ++ timeString ++ " " ++ msg

-- |Message to eventlog.
eventMessage :: MonadIO m => String -> m ()
eventMessage msg = liftIO $ traceEventIO $ "MESSAGE "++msg

logMessage :: MonadProcess m => String -> String -> m ()
logMessage tag msg = do
    pid <- liftP getSelfPid
    liftIO $ traceEventIO $ printf "%s [%s] %s" tag (show pid) msg
