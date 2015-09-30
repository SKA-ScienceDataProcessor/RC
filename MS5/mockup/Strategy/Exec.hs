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
  let go (DomainStep dh) =
        modifyIORef domainMapRef (IM.insert (dhId dh) (AnyDomain dh))

      go (KernelStep _dom kbind) = do

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

      go (SplitStep dh dh0 steps) = do
        putStrLn $ "(splitting domain " ++ show dh0 ++ " into " ++ show dh ++ " not implemented)"
        modifyIORef domainMapRef $ IM.insert (dhId dh) (AnyDomain dh)
        mapM_ go steps

      go (DistributeStep dh _ steps) = do
        putStrLn $ "(distributing domain " ++ show dh ++ " not implemented)"
        mapM_ go steps

  mapM_ go $ runStrategy strat
