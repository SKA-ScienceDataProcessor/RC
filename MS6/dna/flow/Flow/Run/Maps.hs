{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

{- | Common data structures for naive interpreter -}
module Flow.Run.Maps where

import Control.Monad

import Data.Binary
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map

import Flow.Internal

-- | Set of kernels
--
-- This is used in interpretation for reasoning about data
-- dependencies. Note that kernels are synonymous with their output
-- data, so this also identifies a set of buffers we expect to be
-- alive.
type KernelSet = IS.IntSet

-- | Set of domains
--
-- Domain dependencies work the same as kernel dependencies (and
-- should probably be the same on some level)
type DomainSet = IS.IntSet

-- | The data map maps kernel IDs (and therefore buffer IDs) to their
-- data representation (= the return data representation of the
-- kernel) as well as all data we locally have for it. For distributed
-- execution, this will be restricted in the domain that we distribute
-- over.
type DataMap = IM.IntMap (ReprI, RegionData)

-- | Same as "DataMap", except for domains. Note that a split domain
-- is going to be represented as having several regions attached to
-- it.
type DomainMap = IM.IntMap (DomainI, [Region])

writeDataMap :: DataMap -> Put
writeDataMap dm = do
  put (IM.size dm)
  forM_ (IM.assocs dm) $ \(kid, (_, rdata)) -> do
    put kid
    putRegionData rdata

readDataMap :: GetContext -> Get DataMap
readDataMap ctx = do
  size <- get :: Get Int
  IM.fromList <$> replicateM size (do
    kid <- get
    rdata <- getRegionData ctx
    let !repr = case IM.lookup kid (fst ctx) of
          Just kbind -> kernRepr kbind
          Nothing    -> error $ "readDataMap: Unknown kernel ID " ++ show kid ++ "!"
    return (kid, (repr, rdata)))

writeDomainMap :: DomainMap -> Put
writeDomainMap dm = do
  put (IM.size dm)
  forM_ (IM.assocs dm) $ \(did, (DomainI dom, regs)) -> do
    put did
    put (length regs)
    forM_ regs (dhPutRegion dom)

readDomainMap :: GetContext -> Get DomainMap
readDomainMap ctx = do
  size <- get :: Get Int
  IM.fromList <$> replicateM size (do
    did <- get
    regNum <- get
    case IM.lookup did (snd ctx) of
      Just di@(DomainI dom) -> do
        regs <- replicateM regNum (dhGetRegion dom ctx)
        return (did, (di, regs))
      Nothing -> error $ "readDomainMap: Unknown domain ID " ++ show did ++ "!")

adhFilterBox :: DomainI -> RegionBox -> Region -> Maybe Region
adhFilterBox (DomainI dom) = dhFilterBox dom

dataMapInsert :: KernelId -> ReprI -> RegionData -> DataMap -> DataMap
dataMapInsert kid repr rdata = IM.insertWith update kid (repr, rdata)
  where update _ (repr', m) = (repr', Map.union rdata m)

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

-- | Kernel dependencies of a step
stepKernDeps :: Step -> KernelSet
stepKernDeps (DomainStep (Just kid) _)  = IS.singleton kid
stepKernDeps (KernelStep kbind)         = IS.fromList $ map kdepId $ kernDeps kbind
stepKernDeps (RecoverStep kbind kid)    = IS.fromList $ (kid:) $ map kdepId $ kernDeps kbind
stepKernDeps (DistributeStep _ _ steps) = stepsKernDeps steps
stepKernDeps _                          = IS.empty

-- | Kernel dependencies of a series of steps
stepsKernDeps :: [Step] -> KernelSet
stepsKernDeps = stepsKernDeps' IS.empty

-- | Kernel dependencies of a series of steps, with start value
stepsKernDeps' :: KernelSet -> [Step] -> KernelSet
stepsKernDeps' cont (step@(KernelStep kbind) : steps)
  = IS.delete (kernId kbind) $ IS.union (stepKernDeps step) $
    stepsKernDeps' cont steps
stepsKernDeps' cont (step@(RecoverStep kbind _) : steps)
  = IS.delete (kernId kbind) $ IS.union (stepKernDeps step) $
    stepsKernDeps' cont steps
stepsKernDeps' cont (step@(DistributeStep _ _ dsteps) : steps)
  = stepsKernDeps' (stepsKernDeps' cont steps) dsteps
stepsKernDeps' cont (step : steps)
  = stepKernDeps step `IS.union` stepsKernDeps' cont steps
stepsKernDeps' cont []
  = cont

-- | Domain dependencies of a step
stepDomainDeps :: Step -> DomainSet
stepDomainDeps (KernelStep kbind)
  = (IS.fromList $ kernDomain kbind) `IS.union`
    (IS.fromList $ concatMap kdepDomain $ kernDeps kbind)
stepDomainDeps (RecoverStep kbind _)
  = (IS.fromList $ kernDomain kbind) `IS.union`
    (IS.fromList $ concatMap kdepDomain $ kernDeps kbind)
stepDomainDeps (DomainStep _ dh)
  | Just parent <- dhParent dh
  = IS.singleton (dhId parent)
stepDomainDeps (DistributeStep dh _ steps)
  = IS.insert (dhId dh) $ stepsDomainDeps steps
stepDomainDeps _
  = IS.empty

-- | Domain dependencies of a series of steps
stepsDomainDeps :: [Step] -> DomainSet
stepsDomainDeps (step@(DomainStep _ dh) : steps)
  = IS.delete (dhId dh) $ stepDomainDeps step `IS.union` stepsDomainDeps steps
stepsDomainDeps (step : steps)
  = stepDomainDeps step `IS.union` stepsDomainDeps steps
stepsDomainDeps []
  = IS.empty

