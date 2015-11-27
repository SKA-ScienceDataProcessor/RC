{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Flow.Run
  ( dumpStrategy
  , dumpStrategyDOT
  , dumpStep
  , dumpSteps
  , execStrategy
  , execStrategyDNA
  ) where

import Control.Monad
import Control.Applicative
import Control.Concurrent

import Data.List
import Data.Maybe ( fromMaybe, fromJust, mapMaybe )
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Ord
import Data.Time.Clock

import System.IO

import Flow.Internal
import Flow.Builder
import Flow.Vector
import Flow.Run.Maps
import Flow.Run.DNA

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

dumpStep ind (DomainStep m_kid dh)
  = putStrLn $ ind ++ "Domain " ++ show dh ++
               maybe "" (\kid -> " from kernel " ++ show kid) m_kid ++
               maybe "" (\dom -> " split from " ++ show dom) (dhParent dh)
dumpStep ind (KernelStep kb@KernelBind{kernRepr=ReprI rep})
  = putStrLn $ ind ++ "Over " ++ show (reprDomain rep) ++ " run " ++ show kb
dumpStep ind step@(DistributeStep did sched steps)
  = do putStrLn $ ind ++ "Distribute " ++ show did ++ " using " ++ show sched ++
                  " deps " ++ show (stepKernDeps step)
       forM_ steps (dumpStep ("  " ++ ind))

dumpSteps :: Strategy a -> IO ()
dumpSteps strat = do
  forM_ (runStrategy (void strat)) (dumpStep "")

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
      forM_ (Map.assocs m) $ \(rbox, v) -> do
        putStrLn $ "Discarding buffer " ++ show kid ++ " for region box " ++ show rbox
        freeVector v

-- | Execute schedule step
execStep :: IORef DataMap -> IORef DomainMap -> KernelSet -> Step -> IO ()
execStep dataMapRef domainMapRef deps step = case step of
  DomainStep m_kid dh -> do

    -- Look up input data
    dataMap <- readIORef dataMapRef
    let m_buf = do
          kid <- m_kid
          (_rep, bufs) <- IM.lookup kid dataMap
          return bufs

    -- Primitive domains have exactly one region on construction
    regs' <- case dhParent dh of

      -- No parent: Straight-forward creation
      Nothing -> do
        reg <- dhCreate dh (fromMaybe Map.empty m_buf)
        return [reg]

      -- Otherwise: Split an existing domain
      Just parDh -> do

         -- Get domain to split up
        let err = error $ "Could not find domain " ++ show (dhId dh) ++ " to split!"
        (_, regs) <- fromMaybe err . IM.lookup (dhId parDh) <$> readIORef domainMapRef

        -- Perform split
        concat <$> mapM (dhRegion dh) regs

    -- Add to map
    modifyIORef domainMapRef $ IM.insert (dhId dh) (DomainI dh, regs')

  KernelStep kbind@KernelBind{kernRepr=ReprI rep} -> do

    -- Get domains to execute this kernel over
    domainMap <- readIORef domainMapRef
    let lookupDom did = case IM.lookup did domainMap of
          Just dom -> dom
          Nothing  -> error $ "Kernel " ++ show kbind ++ " called for non-existant domain " ++ show did ++ "!"

    -- Construct all output regions (cartesian product of all involved
    -- domain regions)
    let cartProd = sequence :: [[Region]] -> [RegionBox]
        outDoms = map lookupDom (reprDomain rep)
        outRegs = cartProd $ map snd outDoms

    -- Filter boxes (yeah, ugly & slow)
    let filteredRegs :: [RegionBox]
        filteredRegs = foldr filterBox outRegs $ zip [0..] $ map fst outDoms
        filterBox (i, dom) = mapMaybe $ \box ->
          let (pre,reg:post) = splitAt i box
           in case adhFilterBox dom (pre ++ post) reg of
                Just reg' -> Just $ pre ++ reg':post
                Nothing   -> Nothing

    -- Look up input data. Note that we always pass all available data
    -- here. This means that we are relying on
    -- 1) DistributeStep below to restrict the data map
    -- 2) The kernel being able to work with what we pass it
    dataMap <- readIORef dataMapRef
    ins <- forM (kernDeps kbind) $ \kdep -> do
      case IM.lookup (kdepId kdep) dataMap of
        Just (_, bufs) -> return (kdep, bufs)
        Nothing        -> fail $ "Internal error for kernel " ++ show (kernName kbind) ++ ": Input " ++
                                 show (kdepId kdep) ++ " not found!"
           -- This should never happen

    -- Important TODO: Duplicate data that is written here, but read later!

    -- Call the kernel using the right regions
    !t0 <- getCurrentTime
    results <- kernCode kbind (map snd ins) filteredRegs
    !t1 <- getCurrentTime

    -- Check size
    let expectedSizes = map (reprSize rep) filteredRegs
    forM_ (zip results expectedSizes) $ \(res, m_size) ->
      case m_size of
        Just size | size /= vectorByteSize res ->
          fail $ "Kernel " ++ kernName kbind ++ " produced " ++ show (vectorByteSize res) ++
                 " bytes of data, but data representation " ++ show rep ++ " has " ++ show size ++ " bytes!"
        _other -> return ()

    -- Debug
    putStrLn $ "Calculated kernel " ++ show (kernId kbind) ++ ":" ++ kernName kbind ++
               " regions " ++ show filteredRegs ++
               " in " ++ show (1000 * diffUTCTime t1 t0) ++ " ms"

    -- Get inputs that have been written, and therefore should be
    -- considered freed. TODO: ugly
    let writtenIns = filter ((== WriteAccess) . kdepAccess . fst) ins
    forM_ writtenIns $ \(dep, rdata) -> forM_ (Map.keys rdata) $ \rbox ->
      modifyIORef dataMapRef $ flip dataMapDifference $
        dataMapInsert (kdepId dep) (kdepRepr dep) (Map.singleton rbox nullVector) IM.empty

    -- Insert result
    let resultMap = Map.fromList $ zip filteredRegs results
    modifyIORef dataMapRef $ dataMapInsert (kernId kbind) (kernRepr kbind) resultMap

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
      domainMapRef' <- newIORef $ IM.insert (dhId dh) (DomainI dh, [reg]) domainMap'

      -- Also filter data so we only send data for the appropriate region
      let usesRegion (ReprI repr) = dhId dh `elem` reprDomain repr
          filterData (ri, bufs) =
            (ri, if usesRegion ri
                 then Map.filterWithKey (\ds _ -> reg `elem` ds) bufs
                 else bufs)
          dataMap'' = IM.map filterData dataMap'
      dataMapRef' <- newIORef dataMap''

      -- Especially add extra dependencies for all data that is not
      -- local to our region. Otherwise one iteration might end up
      -- discarding data that another needs. (We will discard this
      -- data after the loop in that case).
      let extraDeps = IM.keysSet $ IM.filter (\(ri, _) -> not (usesRegion ri)) dataMap''

      -- Execute steps
      result <- newEmptyMVar
      (if sched == SeqSchedule then id else void . forkOS) $ do
        execSteps dataMapRef' domainMapRef' (deps `IS.union` extraDeps) steps
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
