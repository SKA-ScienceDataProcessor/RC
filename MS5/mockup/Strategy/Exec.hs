{-# LANGUAGE ExistentialQuantification #-}

module Strategy.Exec where

import Control.Monad

import Data.IORef
import qualified Data.IntMap as IM

import Strategy.Builder
import Strategy.Domain
import Strategy.Internal

data AnyDomain = forall a. AnyDomain (DomainHandle a)

execStrategy :: Strategy () -> IO ()
execStrategy strat = do

  -- Execute steps
  dataMapRef <- newIORef IM.empty
  domainMapRef <- newIORef IM.empty
  forM_ (runStrategy strat) $ \step -> case step of

    DomainStep dh -> do
      modifyIORef domainMapRef (IM.insert (dhId dh) (AnyDomain dh))

    KernelStep _dom kbind -> do

      -- Look up input data
      dataMap <- readIORef dataMapRef
      ins <- forM (kernDeps kbind) $ \kid -> case IM.lookup kid dataMap of
        Just inp -> return inp
        Nothing  -> fail $ "Internal error: Input " ++ show kid ++ " not found!"
                    -- This should never happen

      -- Call the kernel
      res <- kernCode kbind ins

      -- Insert result
      modifyIORef dataMapRef (IM.insert (kernId kbind) res)

    _other -> fail "Distribution not implemented!"

  return ()
