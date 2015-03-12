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

-- | Put measurements about execution time of monadic action into
--   eventlog. Result of action is evaluated to WHNF.
timePeriod :: MonadIO m
           => String            -- ^ PID
           -> String            -- ^ Message
           -> m a               --
           -> m a
timePeriod pid ev a = do
    liftIO $ traceEventIO $ "START [" ++ pid ++ "] " ++ ev
    !r <- a
    liftIO $ traceEventIO $ "END [" ++ pid ++ "] " ++ ev
    return r

-- | Put measurements about execution time of monadic action into
--   eventlog. Result of action is evaluated to WHNF.
duration :: MonadProcess m => String -> m a -> m a
duration msg dna = do
    pid <- liftP getSelfPid
    timePeriod (show pid) msg dna


-- | Measure time period of pure computation into eventlog.  It's
--   strict in argument. Because action is pure we put empty PID into
--   eventlog.
timePeriodPure :: String -> a -> a
timePeriodPure ev a = unsafeDupablePerformIO $ do
    traceEventIO ("START [] "++ev)
    a `seq` traceEventIO ("END [] "++ev)
    return a

-- | Synchronize timings - put into eventlog an event with current wall time.
synchronizationPoint :: MonadIO m => String -> m ()
synchronizationPoint msg = liftIO $ do
    utcTime <- getCurrentTime
    -- we are formatting time to number of seconds in POSIX epoch and
    -- fractional part in picoseconds.
    let timeString    = formatTime defaultTimeLocale "%s.%q" utcTime
        humanReadable = formatTime defaultTimeLocale "%F %X" utcTime
    traceEventIO $ "SYNC [] " ++ timeString ++ " " ++ msg
    traceEventIO $ "MSG [] "  ++ "started at " ++ humanReadable

-- | Put message to eventlog.
eventMessage :: MonadIO m => String -> m ()
eventMessage msg =
    liftIO $ traceEventIO $ "MSG [] " ++ msg

-- | Put message to eventlog with custom tag.
taggedMessage :: MonadProcess m
              => String         -- ^ Message tag
              -> String         -- ^ Message
              -> m ()
taggedMessage tag msg = do
    pid <- liftP getSelfPid
    liftIO $ traceEventIO $ printf "%s [%s] %s" tag (show pid) msg
