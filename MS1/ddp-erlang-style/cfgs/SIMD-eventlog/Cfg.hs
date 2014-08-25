-- |Cfg.hs
--
-- Configuration module (executable names, build specific functions, etc).
--
-- Copyright (C) 2014 Braam Research

module Cfg
        ( executableName
        , timePeriod
        , timePeriodPure
        , synchronizationPoint
        ) where

import Control.Monad
import Control.Monad.Trans

import Data.Time

import Debug.Trace

import System.Locale (defaultTimeLocale)

executableName = "ddp-erlang-style-SIMD-eventlog"

-- |Put measurements about time period into eventlog.
timePeriod :: MonadIO m => String -> m a -> m a
timePeriod ev a = do
        liftIO $ traceEventIO $ "START "++ev
        r <- a
        r `seq` return ()
        liftIO $ traceEventIO $ "END "++ev
        return r

-- |Measure time period of pure computation into eventlog.
-- Is strict on argument.
timePeriodPure :: String -> a -> a
timePeriodPure ev a = (traceEvent ("START "++ev) $! a) `seq` (traceEvent ("END "++ev) $! a)

-- |Synchronize timings - put into eventlog an event with current wall time.
synchronizationPoint :: MonadIO m => m ()
synchronizationPoint = liftIO $ do
        utcTime <- getCurrentTime
        let     -- we are formatting time to number of seconds in POSIX epoch and fractional part in picoseconds.
                timeString = formatTime defaultTimeLocale "%s.%q" utcTime
        traceEventIO $ "SYNCHRONIZATION "++timeString
