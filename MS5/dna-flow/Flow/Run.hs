{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Flow.Run
  ( dumpStrategy
  , dumpStrategyDOT
  , dumpStep
  , dumpSteps
  , execStrategy
  ) where

import Control.Monad
import Control.Applicative
import Control.Concurrent

import Data.List
import Data.Maybe ( fromMaybe, fromJust )
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Ord
import Data.Time.Clock
import Data.Typeable

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



dumpStep ind (DomainStep dh)
  = putStrLn $ ind ++ "Domain " ++ show dh
dumpStep ind (SplitStep dh steps)
  = do putStrLn $ ind ++ "Split Domain " ++ show (dhId (fromJust $ dhParent dh)) ++ " into " ++ show dh
       forM_ steps (dumpStep ("  " ++ ind))
dumpStep ind (KernelStep kb@KernelBind{kernRepr=ReprI rep})
  = putStrLn $ ind ++ "Over " ++ show (reprDomain rep) ++ " run " ++ show kb
dumpStep ind step@(DistributeStep did sched steps)
  = do putStrLn $ ind ++ "Distribute " ++ show did ++ " using " ++ show sched ++
                  " deps " ++ show (stepKernDeps step)
       forM_ steps (dumpStep ("  " ++ ind))

dumpSteps :: Strategy a -> IO ()
dumpSteps strat = do
  forM_ (runStrategy (void strat)) (dumpStep "")

data AnyDH = forall a. Typeable a => AnyDH (Domain a)
type DataMap = IM.IntMap (ReprI, Map.Map RegionBox (Vector ()))
type DomainMap = IM.IntMap (AnyDH, RegionBox)

dataMapInsert :: KernelId -> RegionBox -> ReprI -> Vector () -> DataMap -> DataMap
dataMapInsert kid dom repr vec = IM.insertWith update kid (repr, def)
  where def = Map.singleton dom vec
        update _ (repr', m) = (repr', Map.insert dom vec m)

dataMapUnion :: DataMap -> DataMap -> DataMap
dataMapUnion = IM.unionWith combine
  where combine (repr, bufs) (_, bufs') = (repr, bufs `Map.union` bufs')

dataMapUnions :: [DataMap] -> DataMap
dataMapUnions = IM.unionsWith combine
  where combine (repr, bufs) (_, bufs') = (repr, bufs `Map.union` bufs')

dataMapDifference :: DataMap -> DataMap -> DataMap
dataMapDifference = IM.differenceWith remove
  where remove (repr, bufs) (_, bufs')
           = if Map.null diff then Nothing else Just (repr, diff)
          where diff = bufs `Map.difference` bufs'

findParameters :: KernelId -> RegionBox -> DataMap -> IO (Vector (), RegionBox)
findParameters kid inDoms dataMap = case IM.lookup kid dataMap of
  Just (ReprI repr, bufs)
    -- Have it for exactly the right domain region?
    | Just inp <- Map.lookup inDoms bufs
    -> return (inp, inDoms)
    -- Have it for the right domain region, but split into
    -- sub-regions? This is rather ad-hoc, clearly needs a better
    -- solution. We actually have guarantees here - at least if we
    -- ignore the possibility for failure (where in a distributed
    -- implementation parts could be missing).
    | let inps = filter (and . zipWith domainSubset inDoms . fst) $ Map.assocs bufs
    , Just merged <- regionMerge (map fst inps)
    , merged == inDoms
    , (_:_) <- inps
    -> do -- Merge the vectors.
          m_inp' <- reprMerge repr inps inDoms
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
stepKernDeps (KernelStep kbind)         = IS.fromList $ map kdepId $ kernDeps kbind
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
  = IS.fromList $ concatMap kdepDomain $ kernDeps kbind
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
  dataMapRef <- newIORef IM.empty
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
    forM_ (IM.assocs discardMap) $ \(kid, (_repr, m)) ->
      forM_ (Map.assocs m) $ \(dom, v) -> do
        putStrLn $ "Discarding buffer " ++ show kid ++ " for domain " ++ show dom
        freeVector v

-- | Execute schedule step
execStep :: IORef DataMap -> IORef DomainMap -> KernelSet -> Step -> IO ()
execStep dataMapRef domainMapRef deps step = case step of
  DomainStep dh -> do
    -- Primitive domains have exactly one region on construction
    dom <- dhCreate dh
    modifyIORef domainMapRef $ IM.insert (dhId dh) (AnyDH dh, [dom])

  KernelStep kbind@KernelBind{kernRepr=ReprI rep} -> do

    -- Get domains to execute this kernel over
    domainMap <- readIORef domainMapRef
    let lookupDom did = case IM.lookup did domainMap of
          Just dom -> dom
          Nothing  -> error $ "Kernel " ++ show kbind ++ " called for non-existant domain " ++ show did ++ "!"
        outDoms = map lookupDom (reprDomain rep)

    -- Any output domains non-unary and therefore need to be split
    -- into separate kernel calls?
    -- TODO: Some kernels might be okay with producing the output
    -- values for more than one region at the same time. However at
    -- this point we do not have an interface for this yet, so
    -- sequentialising things is the easiest choice that is also
    -- somewhat correct.
    case filter ((/=1) . length . snd) outDoms of
     (AnyDH dh, _):_ ->
      execStep dataMapRef domainMapRef deps $ DistributeStep dh SeqSchedule [step]

     [] -> do
      -- Look up input data
      let lookupUnaryDom = head . snd . lookupDom
      dataMap <- readIORef dataMapRef
      ins <- forM (kernDeps kbind) $ \dep -> do
        par <- findParameters (kdepId dep) (map lookupUnaryDom (kdepDomain dep)) dataMap
        return (dep, par)

      -- Call the kernel using the right regions
      let !outRegs = map (head . snd) outDoms
      !t0 <- getCurrentTime
      res <- kernCode kbind (map snd ins) outRegs
      !t1 <- getCurrentTime

      -- Check size
      case reprSize rep outRegs of
       Just size | size /= vectorByteSize res ->
         fail $ "Kernel " ++ kernName kbind ++ " produced " ++ show (vectorByteSize res) ++
                " bytes of data, but data representation " ++ show rep ++ " has " ++ show size ++ " bytes!"
       _other -> return ()

      -- Debug
      putStrLn $ "Calculated kernel " ++ show (kernId kbind) ++ " regions " ++ show outRegs ++
                 " in " ++ show (diffUTCTime t1 t0) ++ " ms"

      -- Get inputs that have been written, and therefore should be
      -- considered freed. TODO: ugly
      let writtenIns = filter ((== WriteAccess) . kdepAccess . fst) ins
      forM_ writtenIns $ \(dep, (_, dom)) ->
        modifyIORef dataMapRef $ flip dataMapDifference $
          dataMapInsert (kdepId dep) dom (kdepRepr dep) nullVector IM.empty

      -- Insert result
      modifyIORef dataMapRef $ dataMapInsert (kernId kbind) outRegs (kernRepr kbind) res

  SplitStep dh steps -> do

    -- Get domain to split up
    let parDh = fromJust $ dhParent dh
    (_, regs)
      <- fromMaybe (error $ "Could not find domain " ++ show (dhId dh) ++ " to split!") .
         IM.lookup (dhId parDh) <$> readIORef domainMapRef

    -- Perform split
    regs' <- concat <$> mapM (dhRegion dh) regs
    modifyIORef domainMapRef $ IM.insert (dhId dh) (AnyDH dh, regs')

    -- Execute nested steps
    execSteps dataMapRef domainMapRef deps steps

  DistributeStep dh sched steps -> do

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

    -- Distribute over all regions
    let regs = snd $ fromJust $ IM.lookup (dhId dh) domainMap
    threads <- forM regs $ \reg -> do

      -- Make new restricted maps. In a distributed setting, this is
      -- the data we would need to send remotely.
      domainMapRef' <- newIORef $ IM.insert (dhId dh) (AnyDH dh, [reg]) domainMap'

      -- Also filter data so we only send data for the appropriate region
      let filterData (ri@(ReprI repr), bufs)
            | dhId dh `elem` reprDomain repr
            = (ri, Map.filterWithKey (\ds _ -> reg `elem` ds) bufs)
            | otherwise
            = (ri, bufs)
          dataMap'' = IM.map filterData dataMap'
      dataMapRef' <- newIORef dataMap''

      -- Execute steps
      result <- newEmptyMVar
      (if sched == SeqSchedule then id else void . forkOS) $ do
        execSteps dataMapRef' domainMapRef' deps steps
        putMVar result =<< readIORef dataMapRef'
      return (result, dataMap'')

    -- Wait for threads
    dataMapsNew <- mapM readMVar $ map fst threads

    -- Combine maps. What is happening here is:
    --
    -- 1. We add all new buffers that the child returned. Note that it might
    --    return buffers that we already have - this does no harm, but a
    --    distributed version would want to prevent this.
    --
    -- 2. We remove all buffers that the children freed (to prevent double-free).
    --    Obviously only a concern because of SM parallelism.
    let dataMapOrig = dataMapUnions (map snd threads)
        dataMapNew = dataMapUnions dataMapsNew
        dataMapRemoved = dataMapDifference dataMapOrig dataMapNew
    modifyIORef dataMapRef $ dataMapUnion dataMapNew
                           . flip dataMapDifference dataMapRemoved
