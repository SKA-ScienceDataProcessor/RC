
module Flow.Run
  ( dumpStrategy
  , dumpStrategyDOT
  , dumpSteps
  , execStrategy
  ) where

import Control.Monad
import Control.Concurrent

import Data.List
import Data.Maybe ( fromMaybe, fromJust )
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
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
      dump ind step@(DistributeStep did sched steps)
        = do putStrLn $ ind ++ "Distribute " ++ show did ++ " using " ++ show sched ++
                        " deps " ++ show (stepKernDeps step)
             forM_ steps (dump ("  " ++ ind))

  forM_ (runStrategy (void strat)) (dump "")

type DataMap = IM.IntMap (Map.Map [Domain] (Vector (), ReprI))
type DomainMap = IM.IntMap Domain

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

type KernelSet = IS.IntSet
type DomainSet = IS.IntSet

-- | Kernel dependencies of a step
stepKernDeps :: Step -> KernelSet
stepKernDeps (KernelStep kbind)         = IS.fromList $ map fst $ kernDeps kbind
stepKernDeps (SplitStep _ steps)        = stepsKernDeps steps
stepKernDeps (DistributeStep _ _ steps) = stepsKernDeps steps
stepKernDeps _                          = IS.empty

-- | Kernel dependencies of a series of steps
stepsKernDeps :: [Step] -> KernelSet
stepsKernDeps (step@(KernelStep kbind) : steps)
  = IS.delete (kernId kbind) $ stepsKernDeps steps `IS.union` stepKernDeps step
stepsKernDeps (step : steps)
  = stepKernDeps step `IS.union` stepsKernDeps steps
stepsKernDeps []
  = IS.empty

-- | Domain dependencies of a step
stepDomainDeps :: Step -> DomainSet
stepDomainDeps (KernelStep kbind)
  = IS.fromList $ concatMap snd $ kernDeps kbind
stepDomainDeps (SplitStep dh steps)
  = IS.insert (dhId (fromJust (dhParent dh))) $ IS.delete (dhId dh) $ stepsDomainDeps steps
stepDomainDeps (DistributeStep _ _ steps)
  = stepsDomainDeps steps
stepDomainDeps _
  = IS.empty

-- | Domain dependencies of a series of steps
stepsDomainDeps :: [Step] -> DomainSet
stepsDomainDeps (step@(DomainStep dh) : steps)
  = IS.delete (dhId dh) $ stepDomainDeps step `IS.union` stepsDomainDeps steps
stepsDomainDeps (step : steps)
  = stepDomainDeps step `IS.union` stepsDomainDeps steps
stepsDomainDeps []
  = IS.empty

execStrategy :: Strategy () -> IO ()
execStrategy strat = do

  -- Initialise maps
  dataMapRef <- newIORef IM.empty :: IO (IORef DataMap)
  domainMapRef <- newIORef IM.empty

  -- Run steps given by strategy
  execSteps dataMapRef domainMapRef IS.empty $
    runStrategy strat

-- | Execute schedule, discarding unecessary buffers as required
execSteps :: IORef DataMap -> IORef DomainMap -> KernelSet -> [Step] -> IO ()
execSteps dataMapRef domainMapRef topDeps steps = do

  -- Annotate steps with dependencies of the following steps
  let annotated = zip steps $ map stepsKernDeps $ tail $ tails steps
  forM_ annotated $ \(step, deps) -> do

    -- Execute step
    let allDeps = topDeps `IS.union` deps
    execStep dataMapRef domainMapRef allDeps step

    -- Discard all buffers that are not required any more.
    -- TODO: fix O(N^2)
    dataMap <- readIORef dataMapRef
    let isDep kid _ = kid `IS.member` allDeps
        (dataMap', discardMap) = IM.partitionWithKey isDep dataMap
    writeIORef dataMapRef dataMap'
    forM_ (IM.assocs discardMap) $ \(kid,m) ->
      forM_ (Map.assocs m) $ \(dom, (v, _dh)) -> do
        putStrLn $ "Discarding buffer " ++ show kid ++ " for domain " ++ show dom
        freeVector v

-- | Execute schedule step
execStep :: IORef DataMap -> IORef DomainMap -> KernelSet -> Step -> IO ()
execStep dataMapRef domainMapRef deps step = case step of
  DomainStep dh ->
    modifyIORef domainMapRef $ IM.insert (dhId dh) (dhRegion dh 0)

  KernelStep kbind@KernelBind{kernRepr=ReprI rep} -> do

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

  SplitStep dh steps -> do
    modifyIORef domainMapRef $ IM.insert (dhId dh) (dhRegion dh 0)
    execSteps dataMapRef domainMapRef deps steps

  DistributeStep dh sched steps -> do

    when (sched /= SeqSchedule) $
      putStrLn "Only sequential distribution supported, falling back to that!"

    -- Make new domain maps for children
    domainMap <- readIORef domainMapRef
    let domDeps = IS.delete (dhId dh) $ stepDomainDeps step
        isDomDep did _ = did `IS.member` domDeps
        domainMap' = IM.filterWithKey isDomDep domainMap

    -- As well as a new data map
    dataMap <- readIORef dataMapRef
    let dataDeps = stepKernDeps step
        isDataDep did _ = did `IS.member` dataDeps
        dataMap' = IM.filterWithKey isDataDep dataMap

    -- TODO: If we are already distributing over a super-domain,
    -- we do not want to distribute it again here!
    threads <- forM [1..dhSize dh] $ \i -> do

      -- Make new restricted maps. In a distributed setting, this is
      -- the data we would need to send remotely.
      domainMapRef' <- newIORef $ IM.insert (dhId dh) (dhRegion dh i) domainMap'
      dataMapRef' <- newIORef dataMap'

      -- Execute steps
      result <- newEmptyMVar
      void $ forkOS $ do
        execSteps dataMapRef' domainMapRef' deps steps
        putMVar result =<< readIORef dataMapRef'
      return result

    -- Wait for threads, integrate results
    forM_ threads $ \result -> do

      -- Register returned data (all new data should only be for the
      -- distributed domain - check?)
      dataMap'' <- readMVar result
      modifyIORef dataMapRef $ IM.unionWith Map.union dataMap''
