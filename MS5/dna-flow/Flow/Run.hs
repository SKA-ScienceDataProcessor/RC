{-# LANGUAGE ExistentialQuantification #-}

module Flow.Run where

import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe ( fromMaybe, fromJust )
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Ord

import System.IO

import Flow.Internal
import Flow.Builder
import Flow.Vector

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
      dump ind (SplitStep dh steps)
        = do putStrLn $ ind ++ "Split Domain " ++ show (dhId (fromJust $ dhParent dh)) ++ " into " ++ show dh
             forM_ steps (dump ("  " ++ ind))
      dump ind (KernelStep kb@KernelBind{kernRepr=ReprI rep})
        = putStrLn $ ind ++ "Over " ++ show (reprDomain rep) ++ " run " ++ show kb
      dump ind (DistributeStep did sched steps)
        = do putStrLn $ ind ++ "Distribute " ++ show did ++ " using " ++ show sched
             forM_ steps (dump ("  " ++ ind))

  forM_ (runStrategy (void strat)) (dump "")

data AnyDomain = forall a. AnyDomain (DomainHandle a)
type DataMap = IM.IntMap (Map.Map [Domain] (Vector (), ReprI))

findParameters :: KernelId -> [Domain] -> DataMap -> IO (Vector (), [Domain])
findParameters kid inDoms dataMap = case IM.lookup kid dataMap of
  Just m
    -- Have it for exactly the right domain region?
    | Just (inp, _) <- Map.lookup inDoms m
    -> return (inp, inDoms)
    -- Have it for the right domain region, but split into
    -- sub-regions? This is rather ad-hoc, clearly needs a better
    -- solution. We actually have guarantees here - at least if we
    -- ignore the possibility for failure (where in a distributed
    -- implementation parts could be missing).
    | let inps = filter (and . zipWith domainSubset inDoms . fst) $ Map.assocs m
    , Just merged <- domainMerge (map fst inps)
    , merged == inDoms
    , (_, ReprI repr) <- snd $ head inps
    -> do -- Merge the vectors.
          let toMPar (dom, (vec, _repr)) = (dom, vec)
          m_inp' <- reprMerge repr (map toMPar inps) inDoms
          case m_inp' of
            Just inp' -> return (inp', inDoms)
            Nothing   -> fail $ "Could not merge data of representation " ++ show repr ++ "!"
  _other -> fail $ "Internal error: Input " ++ show kid ++
                   " for domains " ++ show inDoms ++ " not found!"
           -- This should never happen

execStrategy :: Strategy () -> IO ()
execStrategy strat = do

  -- Execute steps
  dataMapRef <- newIORef IM.empty :: IO (IORef DataMap)
  domainMapRef <- newIORef IM.empty

  let go (DomainStep dh) =
        modifyIORef domainMapRef $ IM.insert (dhId dh) (dhRegion dh 0)

      go (KernelStep kbind@KernelBind{kernRepr=ReprI rep}) = do

        -- Get domains to execute this kernel over
        domainMap <- readIORef domainMapRef
        let lookupDom did = case IM.lookup did domainMap of
              Just dom -> dom
              Nothing  -> error $
                "Kernel " ++ show kbind ++ " called for domain " ++ show did ++
                " which is neither unary nor distributed over. This is not yet" ++
                " supported!"
            outDoms = map lookupDom (reprDomain rep)

        -- Look up input data
        dataMap <- readIORef dataMapRef
        ins <- forM (kernDeps kbind) $ \(kid, dids) ->
          findParameters kid (map lookupDom dids) dataMap

        -- Call the kernel
        res <- kernCode kbind ins outDoms

        -- Debug
        putStrLn $ "Calculated result for kernel " ++ show (kernId kbind) ++ " domain " ++ show outDoms

        -- Insert result
        let m = fromMaybe Map.empty $ IM.lookup (kernId kbind) dataMap
            m' = Map.insert outDoms (res, kernRepr kbind) m
        writeIORef dataMapRef $ IM.insert (kernId kbind) m' dataMap

      go (SplitStep dh steps) = do
        modifyIORef domainMapRef $ IM.insert (dhId dh) (dhRegion dh 0)
        mapM_ go steps

      go (DistributeStep dh sched steps) = do

        when (sched /= SeqSchedule) $
          putStrLn "Only sequential distribution supported, falling back to that!"

        -- Save previous region to be restored later. TODO: This is ugly as hell
        oldDom <- IM.lookup (dhId dh) <$> readIORef domainMapRef

        -- TODO: If we are already distributing over a super-domain,
        -- we do not want to distribute it again here!
        forM_ [1..dhSize dh] $ \i -> do
          modifyIORef domainMapRef (IM.insert (dhId dh) (dhRegion dh i))
          mapM_ go steps

        -- Restore previous version
        case oldDom of
          Just dom -> modifyIORef domainMapRef (IM.insert (dhId dh) dom)
          Nothing -> return ()

  mapM_ go $ runStrategy strat
