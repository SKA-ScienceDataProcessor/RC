-- | Utilities for stress testing DNA code
module DNA.Interpreter.Testing where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import System.Random
import DNA.Types

-- | Crash process 
crashMaybe :: MonadProcess m => Double -> m ()
crashMaybe pCrash = do
    roll <- liftIO randomIO
    me   <- liftP  getSelfPid
    when (roll < pCrash) $ do
        liftIO $ threadDelay (500*1000)
        liftIO $ putStrLn $ show me ++ " CRASH!"
        error "Ooops crashed"
