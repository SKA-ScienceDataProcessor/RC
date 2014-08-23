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

executableName = "ddp-erlang-style"

-- |Put measurements about time period into eventlog or do nothing, as in this case.
timePeriod :: MonadIO m => String -> m a -> m a
timePeriod _ io = io

-- |Measure time period of pure computation into eventlog or do nothing, as in this case.
-- Might be too strict on argument.
timePeriodPure :: String -> a -> a
timePeriodPure _ = id

-- |Synchronize timings - put into eventlog an event with current wall time.
synchronizationPoint :: MonadIO m => m ()
synchronizationPoint = return ()
