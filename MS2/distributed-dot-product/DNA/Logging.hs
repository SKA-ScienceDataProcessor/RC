{-# LANGUAGE RankNTypes #-}
-- | Logging.hs
--
-- Logging facilities.
--
-- Copyright (C) 2014 Braam Research, LLC.
module DNA.Logging where


-- | Dictionary of logging functions. Note that logger could implement
--   only parts of described functionality.
data Logger m = Logger
  { -- | Measure how long action will execute and put it into eventlog.
    timePeriod :: forall a. String -> m a -> m a
  , -- | Measure how long does pure computation take to compute and put it into eventlog.
    timePeriodPure :: forall a. String -> a -> a
  , -- | Synchronize timings - put into eventlog an event with current wall time.
    synchronizationPoint :: m ()
  , -- | Put message to event log
    eventMessage :: String -> m ()
  }
