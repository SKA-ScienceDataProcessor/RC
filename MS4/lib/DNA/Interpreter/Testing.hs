-- | Utilities for stress testing DNA code
module DNA.Interpreter.Testing where

import Control.Monad
import Control.Monad.Reader
-- import Control.Monad.IO.Class
-- import Control.Concurrent (threadDelay)
import Control.Distributed.Process
-- import System.Random
import DNA.Interpreter.Types
import DNA.Types
import System.IO.Unsafe
import Data.IORef

{-
-- | Crash process 
crashMaybe :: MonadProcess m => Double -> m ()
crashMaybe pCrash = do
    roll <- liftIO randomIO
    me   <- liftP  getSelfPid
    when (roll < pCrash) $ do
        liftIO $ threadDelay (0*1000)
        liftIO $ putStrLn $ show me ++ " CRASH!"
        error "Ooops crashed"
-}


ref :: IORef Bool
ref = unsafePerformIO $ newIORef False
{-# NOINLINE ref #-}

-- | Crash process 
crashMaybeWorker :: Double -> DnaMonad ()
crashMaybeWorker _ = do
    n <- envRank `fmap` ask 
    f <- liftIO $ readIORef ref
    me <- liftP getSelfPid
    when (not f && n==0) $ do
        liftIO $ writeIORef ref True
        liftIO $ putStrLn $ show me ++ " CRASH!"
        error "Ooops crashed"
