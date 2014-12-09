{-# LANGUAGE RankNTypes #-}
-- | Logging.hs
--
-- Logging facilities.
--
-- Copyright (C) 2014 Braam Research, LLC.
module DNA.Logging where


import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable (Serializable)
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import qualified Data.Foldable   as T
import System.IO
import System.Directory   (createDirectoryIfMissing)
import GHC.Generics (Generic)



-- | Start logger process and register in local registry
startLoggerProcess :: FilePath -> Process ()
startLoggerProcess logdir = do
    liftIO $ createDirectoryIfMissing True logdir
    bracket open fini $ \h -> do
        me <- getSelfPid
        register "dnaLogger" me
        forever $ do
            s <- expect
            liftIO $ hPutStrLn h s
            liftIO $ hFlush h
  where
    open   = liftIO (openFile (logdir ++ "/log") WriteMode)
    fini h = liftIO (hClose h)


{-
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
-}
