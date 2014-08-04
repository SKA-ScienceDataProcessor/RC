-- |Cfg.hs
--
-- Configuration module (executable names, build specific functions, etc).
--
-- Copyright (C) 2014 Braam Research

module Cfg
	( executableName
	, event
	) where

import Control.Monad
import Control.Monad.Trans

import Debug.Trace

executableName = "ddp-erlang-style-SIMD-eventlog"

event :: MonadIO m => String -> m a -> m a
event ev a = do
	liftIO $ traceEventIO $ "START "++ev
	r <- a
	liftIO $ traceEventIO $ "END "++ev
	return r
