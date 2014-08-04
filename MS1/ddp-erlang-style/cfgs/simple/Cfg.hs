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

executableName = "ddp-erlang-style"

event :: MonadIO m => String -> m a -> m a
event _ io = io
