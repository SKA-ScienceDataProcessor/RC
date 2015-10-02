{-# LANGUAGE ExistentialQuantification #-}

module Flow.Run where

import Control.Monad

import Data.List
import qualified Data.IntMap as IM
import Data.IORef
import Data.Ord

import System.IO

import Flow.Internal
import Flow.Builder

dumpStrategy :: Strategy a -> IO ()
dumpStrategy strat = do

  -- Construct strategy map
  let kerns = runStrategy (void strat)

  -- Generate sorted kernel
  let kerns' = sortBy (comparing kernId) (stepsToKernels kerns)

  let dumpKern (KernelBind kid kfl kname repr deps _ _) = do
        putStrLn $ concat
          [ "Kernel ", show kid, ":", kname, " implementing ", flName kfl
          , " producing ", show repr, " using ", show deps ]
  forM_ kerns' dumpKern

dumpStrategyDOT :: FilePath -> Strategy a -> IO ()
dumpStrategyDOT file strat = do

  -- Construct strategy map
  let kerns = runStrategy (void strat)

  -- Generate sorted kernel
  let kerns' = sortBy (comparing kernId) (stepsToKernels kerns)

  -- Open output file
  h <- openFile file WriteMode
  hPutStrLn h "digraph strategy {"
  let kidName kid = "kernel" ++ show kid
  let dumpKern (KernelBind kid kfl kname repr deps _ _) = do
        hPutStrLn h $ concat
          [ kidName kid, " [label=\"" ++ kname, " implementing ", flName kfl,
            " producing ", show repr, "\"]"]
        forM_ (deps) $ \kid' ->
          hPutStrLn h $ concat
            [ kidName kid', " -> ", kidName kid]
  forM_ kerns' dumpKern
  hPutStrLn h "}"
  hClose h

dumpSteps :: Strategy a -> IO ()
dumpSteps strat = do

  let dump ind (DomainStep dh)
        = putStrLn $ ind ++ "Domain " ++ show dh
      dump ind (SplitStep dh' dh steps)
        = do putStrLn $ ind ++ "Split Domain " ++ show (dhId dh) ++ " into " ++ show dh'
             forM_ steps (dump ("  " ++ ind))
      dump ind (KernelStep dids kb)
        = putStrLn $ ind ++ "Over " ++ show dids ++ " " ++ show kb
      dump ind (DistributeStep did sched steps)
        = do putStrLn $ ind ++ "Distribute " ++ show did ++ " using " ++ show sched
             forM_ steps (dump ("  " ++ ind))

  forM_ (runStrategy (void strat)) (dump "")

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
