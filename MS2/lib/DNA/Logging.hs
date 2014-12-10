{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
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
import Control.Exception  (SomeException)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable (Serializable)
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Foldable   as T
import System.IO
import System.Directory   (createDirectoryIfMissing)
import GHC.Generics (Generic)
import Text.Printf


----------------------------------------------------------------
-- Message data types for logger
----------------------------------------------------------------

-- | Time stamp in POSIX time. We have microsecond precision.
type TimeStamp = Double

-- | Message data type which is sent to the logger process
data LogMsg
    = LogMsg    Double ProcessId String String
      -- ^ Time stamp, source process, tag, message
    | SyncPoint Double
    deriving (Show,Eq,Ord,Typeable,Generic)
instance Binary LogMsg

-- | Create log message to send to logger
makeLogMessage :: String -> String -> Process LogMsg
makeLogMessage tag msg = do
    t  <- liftIO getPOSIXTime
    me <- getSelfPid
    return $ LogMsg (realToFrac t) me tag msg


----------------------------------------------------------------
-- Logger process
----------------------------------------------------------------

-- | Start logger process and register in local registry
startLoggerProcess :: FilePath -> Process ()
startLoggerProcess logdir = do
    liftIO $ createDirectoryIfMissing True logdir
    bracket open fini $ \h -> do
        me <- getSelfPid
        register "dnaLogger" me
        forever $ do
            msg <- expect
            catch (
             case msg of
              LogMsg t pid tag s ->
                  liftIO $ hPutStrLn h $ printf "%f %s [%s]: %s" t (show pid) tag s
              -- FIXME: write it
              SyncPoint{} -> return ()
             ) (\e -> liftIO $ print (e :: SomeException))
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
