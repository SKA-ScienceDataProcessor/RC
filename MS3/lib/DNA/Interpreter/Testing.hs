-- | Utilities for stress testing DNA code
module DNA.Interpreter.Testing where

import Control.Monad
import Control.Monad.IO.Class
import System.Random

-- | Crash process 
crashMaybe :: MonadIO m => Double ->m ()
crashMaybe pCrash = liftIO $ do
    roll <- liftIO randomIO
    when (roll < pCrash) $ error "Ooops crashed"
    
