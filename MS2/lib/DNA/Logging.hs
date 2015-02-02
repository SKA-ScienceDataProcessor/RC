{-# LANGUAGE BangPatterns #-}
-- | Logging.hs
--
-- Logging facilities. Log messages are written to GHC's eventlog in
-- following format:
--
-- > TAG [PID] message
--
-- Tag is sequence of of alphanumeric characters. Usually it's all
-- caps. PID is cloud haskell process's ID and enclosed in square
-- brackets. For messages about whole program it's set to empty
-- string. Message is free form.
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

taggedMessage :: MonadProcess m => String -> String -> m ()
taggedMessage tag msg = do
    pid <- liftP getSelfPid
    liftIO $ traceEventIO $ printf "%s [%s] %s" tag (show pid) msg
