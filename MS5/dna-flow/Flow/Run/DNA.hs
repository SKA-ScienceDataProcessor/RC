{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Flow.Run.DNA
  ( execStrategyDNA
  ) where

import Control.Monad
import Control.Monad.State.Strict
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable
import Control.Distributed.Static

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Maybe ( fromMaybe, fromJust, mapMaybe )
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set    as Set
import Data.Rank1Dynamic ( toDynamic )
import qualified Data.Map.Strict as Map
import Data.Typeable

import Flow.Internal
import Flow.Builder hiding (kernel)
import Flow.Vector
import Flow.Run.Maps

import DNA

-- * DNA wrapper monad

-- | Tracks runtime data while we are executing the DNA program.
-- The result of an actor will be the "DataMap" at the end of its
-- execution. A proper compiler would be able to statically deduce
-- what elements of "DomainMap" and "DataMap" will be set at every
-- given moment, but for simplicity we skip this here.
--
-- In practice, this relies on
--
--  1. The executed "Step"s never using a kernel or domain before it
--     was defined
--
--  2. The fact that "DistributeStep" will check the dependencies of
--     the executed steps when restricting the data map
--
--  3. When cleaning up data between steps we also look ahead to the
--     remaining steps to make sure we are not discarding anything
--     that will be required later on.
type DnaCode a = StateT (DomainMap, DataMap) DNA a

-- | Helper for getting the current data map at run time
getDomainMap :: DnaCode DomainMap
getDomainMap = fst <$> get

-- | Helper for setting the current domain map at run time
putDomainMap :: DomainMap -> DnaCode ()
putDomainMap dm' = modify (\(_, dm) -> (dm', dm))

-- | Helper for setting the current data map at run time
putDataMap :: DataMap -> DnaCode ()
putDataMap dm' = modify (\(dm, _) -> (dm, dm'))

-- | Helper for getting the current data map at run time
getDataMap :: DnaCode DataMap
getDataMap = snd <$> get

-- | Helper for modifiying the current data map at run time
modifyDataMap :: (DataMap -> DataMap) -> DnaCode ()
modifyDataMap f = putDataMap . f =<< getDataMap

-- | Shortcut for lifting "IO" into the "DnaCode" monad using a named
-- "kernel".
execIO :: String -> IO a -> DnaCode a
execIO name = lift . kernel name [] . liftIO

-- * DNA builder monad

-- | State of the "DnaBuilder". This tracks all information we need
-- while generating "DnaCode".
data DnaBuildState = DnaBuildState
  { dbsFresh          :: !Int           -- ^ Fresh number source for generating static names
  , dbsRemoteRegister :: RemoteRegister -- ^ Remote table in construction
  , dbsContext        :: GetContext     -- ^ Static domain and kernel maps (for unmarshaling)
  , dbsCode           :: DnaCode ()
     -- ^ Collected "DnaCode" for the current actor / top level
     -- routine. When generating an actor, the "DataMap" after the
     -- last step's cleanup is the result of the actor. The way our
     -- cleanup system works, this will be exactly the dependencies
     -- that were passed into "execSteps" below.
  }

type DnaBuilder a
  = State DnaBuildState a

-- | Emit "DnaCode" into the curret actor / top level routine. Use the
-- "DnaCode" state monad to access current domains or data.
emitCode :: DnaCode () -> DnaBuilder ()
emitCode code = modify $ \dbs ->
  dbs { dbsCode = dbsCode dbs >> code }

-- | Register a kernel. This is required so we can recognise the ID
-- when unmarshaling.
registerKernel :: KernelBind -> DnaBuilder ()
registerKernel kbind = do
   dbs <- get
   let kernelCtx = IM.insert (kernId kbind) kbind (fst (dbsContext dbs))
   put $ dbs { dbsContext = (kernelCtx, snd (dbsContext dbs)) }

-- | Register a domain. This is required so we can recognise the ID
-- when unmarshaling.
registerDomain :: Typeable a => Domain a -> DnaBuilder ()
registerDomain dom = do
   dbs <- get
   let domainCtx = IM.insert (dhId dom) (DomainI dom) (snd (dbsContext dbs))
   put $ dbs { dbsContext = (fst (dbsContext dbs), domainCtx) }

-- | Register an actor with Cloud Haskell. This generates a
-- @DNA@-compatible "Closure" for the actor and registers it in the
-- remote table to be passed to "dnaRun".
registerActor :: (Serializable a, Serializable b)
              => Actor a b -> DnaBuilder (Closure (Actor a b))
registerActor act = do
  dbs <- get
  -- Register using low-level rank1dynamic / distributed-static. Bit
  -- strange that this doesn't work using CH's primitives...
  let name = "dna_static_" ++ show (dbsFresh dbs)
      reg = registerStatic name (toDynamic act)
      actClosure = staticClosure (staticLabel name)
  put dbs { dbsFresh = dbsFresh dbs + 1
          , dbsRemoteRegister = dbsRemoteRegister dbs . reg
          }
  return actClosure

execStrategyDNA :: Strategy () -> IO ()
execStrategyDNA strat = do

  -- Convert steps given by strategy into DNA
  let steps = runStrategy strat
      dbs = DnaBuildState
            { dbsFresh          = 0
            , dbsRemoteRegister = id
            , dbsContext        = (IM.empty, IM.empty)
            , dbsCode           = return ()
            }
      (_, state') = runState (execSteps IS.empty steps) dbs

  -- Actually run it!
  dnaRun (dbsRemoteRegister state') $
    flip evalStateT (IM.empty, IM.empty) $
      dbsCode state'

-- | Generate code for schedule. The kernel set passed in is the data
-- dependencies we promise to keep alive at the end of the generated
-- code. For generating actor code, this should therefore be the
-- actor's return values.
execSteps :: KernelSet -> [Step] -> DnaBuilder ()
execSteps topDeps steps = do

  -- Annotate steps with dependencies of the following steps
  let annotated = zip steps $ map stepsKernDeps $ tail $ tails steps
  forM_ annotated $ \(step, deps) -> do

    -- Compile step
    let allDeps = topDeps `IS.union` deps
    execStep allDeps step

    -- Discard all buffers that are not required any more.
    -- TODO: fix O(N^2)
    emitCode $ do
      (dataMap) <- getDataMap
      let isDep kid _ = kid `IS.member` allDeps
          (dataMap', discardMap) = IM.partitionWithKey isDep dataMap
      putDataMap dataMap'
      forM_ (IM.assocs discardMap) $ \(kid, (_repr, m)) ->
        forM_ (Map.assocs m) $ \(rbox, v) -> lift $ do
          logMessage $ "Discarding buffer " ++ show kid ++ " for region box " ++ show rbox
          kernel ("discard" ++ show kid) [] $ liftIO $
            freeVector v

-- | Generate code for schedule step. As before, the kernel set is all
-- kernel results we want to keep alive. In contrast to "execSteps",
-- this time around this includes the data dependencies of following
-- steps.
execStep :: KernelSet -> Step -> DnaBuilder ()
execStep deps step = case step of
  DomainStep m_kid dh
    -> execDomainStep m_kid dh
  KernelStep kbind
    -> execKernelStep kbind
  DistributeStep dom sched steps
    -> execDistributeStep deps dom sched steps
  RecoverStep kbind kid
    -> execRecoverStep kbind kid


-- | Generate code for creating a domain. This will register the
-- domain for unmarshalling, and generate "DNA" code to register the
-- domain with the run-time "DomainMap".
execDomainStep :: Typeable a => Maybe KernelId -> Domain a -> DnaBuilder ()
execDomainStep m_kid dh = do

  -- Save domain for unmarshaling
  registerDomain dh
  emitCode $ do

    -- Look up input data
    dataMap <- getDataMap
    let m_buf = do
          kid <- m_kid
          (_rep, bufs) <- IM.lookup kid dataMap
          return bufs

    -- Primitive domains have exactly one region on construction
    regs' <- case dhParent dh of

      -- No parent: Straight-forward creation
      Nothing -> do
        reg <- execIO "create domain" $ dhCreate dh (fromMaybe Map.empty m_buf)
        return [reg]

      -- Otherwise: Split an existing domain
      Just parDh -> do

         -- Get domain to split up
        let err = error $ "Could not find domain " ++ show (dhId dh) ++ " to split!"
        (_, regs) <- fromMaybe err . IM.lookup (dhId parDh) <$> getDomainMap

        -- Perform split
        concat <$> execIO "split domain" (mapM (dhRegion dh) regs)

    -- Add to map
    dm <- getDomainMap
    putDomainMap $ IM.insert (dhId dh) (DomainI dh, regs') dm


filteredRegsFun :: [RegionBox] -> [(DomainI,a)] -> [RegionBox]
filteredRegsFun outRegs outDoms
  = foldr filterBox outRegs $ [0..] `zip` map fst outDoms
  where
    filterBox (i, dom) = mapMaybe $ \box ->
      let (pre,reg:post) = splitAt i box
      in case adhFilterBox dom (pre ++ post) reg of
           Just reg' -> Just $ pre ++ reg':post
           Nothing   -> Nothing

getFilteredOutputRegs :: ReprI -> String -> DnaCode [RegionBox]
getFilteredOutputRegs (ReprI rep) kname = do
  -- Get domains to execute this kernel over
  domainMap <- getDomainMap
  let lookupDom did = case IM.lookup did domainMap of
        Just dom -> dom
        Nothing  -> error $ "Kernel " ++ kname ++ " called for non-existant domain " ++ show did ++ "!"
  -- Construct all output regions (cartesian product of all involved
  -- domain regions)
  let cartProd = sequence :: [[Region]] -> [RegionBox]
      outDoms = map lookupDom (reprDomain rep)
      outRegs = cartProd $ map snd outDoms
  -- Filter boxes (yeah, ugly & slow)
  return $ filteredRegsFun outRegs outDoms


-- | Generate code for executing a kernel. This will register the
-- kernel for unmarshalling, and generate "DNA" code to locate the
-- appropriate data in the run-time "DataMap", invoke the kernel code
-- and update it with the results accordingly.
execKernelStep :: KernelBind -> DnaBuilder ()
execKernelStep kbind@KernelBind{kernRepr=ReprI rep} = do
  registerKernel kbind
  emitCode $ do
    filteredRegs <- getFilteredOutputRegs (ReprI rep) (show kbind)
    -- Look up input data. Note that we always pass all available data
    -- here, we do not check whether it satisfies the usual "if split
    -- & distributed domains are used, only pass the current region"
    -- specification. We are relying on execDistributeStep below to
    -- organise the data maps accordingly.
    dataMap <- getDataMap
    ins <- forM (kernDeps kbind) $ \kdep -> do
      case IM.lookup (kdepId kdep) dataMap of
        Just (_, bufs) -> return (kdep, bufs)
        Nothing        -> fail $ "Internal error for kernel " ++ show (kernName kbind) ++ ": Input " ++
                                 show (kdepId kdep) ++ " not found!"
           -- This should never happen

    -- Important TODO: Duplicate data that is written here, but read later!

    -- Call the kernel using the right regions
    results <- lift $ kernel (kernName kbind) (kernHints kbind)
             $ liftIO $ kernCode kbind (map snd ins) filteredRegs

    -- Check size
    let expectedSizes = map (reprSize rep) filteredRegs
    forM_ (zip results expectedSizes) $ \(res, m_size) ->
      case m_size of
        Just size | size /= vectorByteSize res ->
          fail $ "Kernel " ++ kernName kbind ++ " produced " ++ show (vectorByteSize res) ++
                 " bytes of data, but data representation " ++ show rep ++ " has " ++ show size ++ " bytes!"
        _other -> return ()

    -- Debug
    lift $ logMessage $
      "Calculated kernel " ++ show (kernId kbind) ++ ":" ++ kernName kbind ++
      " regions " ++ show filteredRegs

    -- Get inputs that have been written, and therefore should be
    -- considered freed. TODO: ugly
    let writtenIns = filter ((== WriteAccess) . kdepAccess . fst) ins
    forM_ writtenIns $ \(dep, rdata) -> forM_ (Map.keys rdata) $ \rbox -> do
      modifyDataMap $ flip dataMapDifference $
        dataMapInsert (kdepId dep) (kdepRepr dep) (Map.singleton rbox nullVector) IM.empty

    -- Insert result
    let resultMap = Map.fromList $ zip filteredRegs results
    modifyDataMap $ dataMapInsert (kernId kbind) (kernRepr kbind) resultMap


execRecoverStep :: KernelBind -> KernelId -> DnaBuilder ()
execRecoverStep kbind kid = do
  registerKernel kbind
  emitCode $ do
    -- Actual data
    dataMap        <- getDataMap
    (repr,regData) <- case kid `IM.lookup` dataMap of
      Nothing -> undefined
      Just a  -> return a
    -- Expected regions
    filteredRegs <- getFilteredOutputRegs repr ("Recover: " ++ show kbind)
    -- Compare regions
    let missing = Set.fromList filteredRegs `Set.difference` Map.keysSet regData
    unless (Set.null missing) $ do
      -- Generate missing regions
      results <- forM (Set.toList missing) $ \rbox -> do
        [res] <- lift $ kernel (kernName kbind) (kernHints kbind)
               $ liftIO $ kernCode kbind [] [rbox]
        return (rbox,res)
      -- Update map
      modifyDataMap $ flip IM.adjust kid $ \(r,dm) ->
        (r, foldl' (\m (k,v) -> Map.insert k v m) dm results)




-- | Generate code for distributing a number of scheduled steps.
--
-- This means that we will generate a new actor for the contained code
-- (see "makeActor") and generate code for restricting data and domain
-- maps appropriately for the sub-steps. After the actors are finished
-- they return the (marshalled) "DataMap"s with the requested data
-- buffers, which we re-integrate into our own "DataMap".
execDistributeStep :: Typeable a => KernelSet -> Domain a -> Schedule -> [Step] -> DnaBuilder ()
execDistributeStep deps dh sched steps = do

  -- Generate nested (actor) code.
  --
  -- The data dependencies passed is the kernel data that the actors
  -- are going to send back in their "DataMap"s. This should be all
  -- data that the following steps depend on - which is exactly the
  -- difference between our "deps" and the dependencies of the nested
  -- steps.
  let rets = deps `IS.difference` stepsKernDeps steps
  (act, marshal, unmarshal) <- makeActor rets steps

  emitCode $ do

    -- Make new domain maps for children
    domainMap <- getDomainMap
    let domDeps = IS.delete (dhId dh) $ stepsDomainDeps steps
        isDomDep did _ = did `IS.member` domDeps
        domainMap' = IM.filterWithKey isDomDep domainMap

    -- As well as a new data map
    dataMap <- getDataMap
    let dataDeps = stepsKernDeps steps
        isDataDep did _ = did `IS.member` dataDeps
        dataMap' = IM.filterWithKey isDataDep dataMap

    -- Generate maps for all regions. This is the data that needs to
    -- be sent to the actors.
    let regs = snd $ fromJust $ IM.lookup (dhId dh) domainMap
    inputs <- forM regs $ \reg -> do

      -- Make new restricted maps.
      let domainMap'' = IM.insert (dhId dh) (DomainI dh, [reg]) domainMap'

      -- Also filter data so we only send data for the appropriate region
      let usesRegion (ReprI repr) = dhId dh `elem` reprDomain repr
          filterData (ri, bufs) =
            (ri, if usesRegion ri
                 then Map.filterWithKey (\ds _ -> reg `elem` ds) bufs
                 else bufs)
          dataMap'' = IM.map filterData dataMap'

      -- Execute steps with new domain & data maps
      return $ marshal domainMap'' dataMap''

    -- Calculate the number of nodes needed. Note that this only works
    -- if the domains in questions are split *before* the
    -- distribution, otherwise we cannot predict resource
    -- demands. This is sort-of by design.
    let stepNodes (DistributeStep dom ParSchedule ss)
          | Just (_, rs) <- IM.lookup (dhId dom) domainMap
          = length rs * stepsNodes ss
        stepNodes _ = 1
        stepsNodes = maximum . map stepNodes

    dataMapNew <- case sched of
     ParSchedule -> lift $ do

       -- Make actor group. Allocation as calculated above, and we always
       -- include the local node, as we are going to block.
       let nodes = stepsNodes steps
           totalNodes = length inputs * nodes
       grp <- startGroup (N (totalNodes - 1)) (NNodes nodes) $ do
         -- FIXME: We always make use of same node as parent which may
         --        not be good idea in all cases. But scheduling in
         --        DNA is bad overall
         useLocal
         failout
         return act

       -- Send out inputs
       distributeWork inputs (const id) grp

       -- Delay, collect & unmarshal result data maps
       promise <- delayGroup grp
       let collect dm out = dm `dataMapUnion` unmarshal out
       gather promise collect IM.empty

     SeqSchedule -> do

       -- Simply use evalClosure to evaluate actors directly
       dataMapUnions <$> forM inputs (\inp -> unmarshal <$> (lift $ evalClosure act inp))

    -- Combine maps.
    modifyDataMap $ dataMapUnion dataMapNew

-- | Generate an actor executing the given steps. The result will be a
-- (marshalled) "DataMap" containing data for the kernels mentioned in
-- the "KernelSet".
--
-- We also return the marshaling and unmarshaling routines to use with
-- the actor - mainly so all all marshalling code is in one place.
makeActor :: KernelSet -> [Step]
          -> DnaBuilder (Closure (Actor LBS.ByteString LBS.ByteString),
                         DomainMap -> DataMap -> LBS.ByteString,
                         LBS.ByteString -> DataMap)
makeActor rets steps = do

  -- Generate code for steps.
  dbs <- get
  let dbs' = dbs { dbsCode = return () }
      dbs'' = flip execState dbs' $
              execSteps rets steps
      ctx = dbsContext dbs''

  -- Take over all state, but reset code to where it was before.
  put $ dbs'' { dbsCode = dbsCode dbs }

  -- Generate actor code
  let act :: Actor LBS.ByteString LBS.ByteString
      act = actor $ \inp -> do

        -- Unmarshal argument maps
        let (domainMap, dataMap) = flip runGet inp (
              (,) <$> readDomainMap ctx <*> readDataMap ctx)

        -- Run code
        (_, dataMapNew) <- execStateT (dbsCode dbs'') (domainMap, dataMap)

        -- Marshal result. "execSteps" should have generated code such
        -- that at this point only data mentioned in "rets" is still
        -- left alive.
        return $ runPut $ writeDataMap dataMapNew

  -- Register in remote table
  actClosure <- registerActor act

  -- Generate actor calling code
  let marshal domainMap dataMap = runPut $
            writeDomainMap domainMap >> writeDataMap dataMap
      unmarshal = runGet (readDataMap ctx)
  return (actClosure, marshal, unmarshal)
