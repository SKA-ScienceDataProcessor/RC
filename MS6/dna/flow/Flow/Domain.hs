{-# LANGUAGE DeriveDataTypeable #-}

module Flow.Domain
  ( Domain, Region, RegionBox
  , Schedule(..)
  , Range, makeRangeDomain, regionRange
  , Bins, makeBinDomain, regionBins, RegionBinDesc(..)
  , split, distribute
  ) where

import Control.Monad
import Control.Monad.State.Strict (state, runState, modify)
import qualified Control.Monad.State.Strict as State

import Data.Binary
import Data.Int
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Maybe ( mapMaybe )
import Data.Typeable

import Flow.Internal
import Flow.Builder
import Flow.Vector

-- | Create a new range domain, with one region spanning the range
-- @[low,high[@.
makeRangeDomain :: Int -> Int -> Strategy (Domain Range)
makeRangeDomain rlow rhigh = do
  d <- makeRangeDomain' rlow rhigh 1 Nothing 1
  addStep $ DomainStep Nothing d
  return d

makeRangeDomain' :: Int -> Int -> Int -> Maybe (Domain Range) -> Int -> Strategy (Domain Range)
makeRangeDomain' rlow rhigh size parDh nsplit = do
  did' <- freshDomainId
  let region (RangeRegion _ (Range low high)) i
        = Range (low + (high-low) * i `div` nsplit)
                (low + (high-low) * (i+1) `div` nsplit)
      region _ _ = error "makeRangeDomain': impossible"
  let dh' = Domain
       { dhId     = did'
       , dhSplit  = makeRangeDomain' rlow rhigh (size*nsplit) (Just dh')
       , dhParent = parDh
       , dhCreate = const $ return $ RangeRegion dh' (Range rlow rhigh)
       , dhRegion = \d -> return $ map (RangeRegion dh' . region d) [0..nsplit-1]
       , dhRestrict = restrictRangeRegion
       , dhFilterBox = \_ -> Just -- no dependencies
       , dhPutRegion = putRangeRegion
       , dhGetRegion = getRangeRegion dh'
       }
  return dh'

restrictRangeRegion :: Region -> [Region] -> [Region]
restrictRangeRegion (RangeRegion _ (Range low high)) = filter restrict
  where restrict (RangeRegion _ (Range l h)) = l >= low && h <= high
        restrict _                           = False
restrictRangeRegion other
  = error $ "restrictRangeRegion: " ++ show other ++ " is no range region!"

putRangeRegion :: Region -> Put
putRangeRegion (RangeRegion _ (Range low high)) = do
  put low
  put high
putRangeRegion other =
  fail $ "putRangeRegion: " ++ show other ++ " is no range region!"

getRangeRegion :: Domain Range -> GetContext -> Get Region
getRangeRegion dom _ = do
  low <- get
  high <- get
  return $ RangeRegion dom (Range low high)

-- | Create a new bin domain from data produced by the given
-- kernel. The data format should be the same as an array of C
-- structs, each describing a bin as follows:
--
--  > struct { double low, high; uint64_t count; }
--
-- @low@ and @high@ describes the covered value range @[low,high[@ of
-- the binning function. Individual bins must not overlap in their
-- value range. @count@ is the number of data points that fall into
-- this range (= the size of the bin).
makeBinDomain :: Kernel a -> Strategy (Domain Bins)
makeBinDomain kern = do

  -- Bind an internal kernel flow
  Flow dflow <- bindNew kern
  ss <- Strategy State.get
  case HM.lookup dflow (ssMap ss) of
    Nothing -> fail $ "makeBinDomain: Internal error - failed to look up flow " ++ show dflow ++ "!"
    Just k -> do
      did' <- freshDomainId
      let dh' :: Domain Bins
          dh' = Domain
           { dhId     = did'
           , dhSplit  = makeBinSubDomain dh'
           , dhParent = Nothing
           , dhCreate = unpackBinRegion dh'
           , dhRegion = fail "dhRegion called on bin root domain!"
           , dhRestrict = restrictBinRegion
           , dhFilterBox = filterBoxBin
           , dhPutRegion = putBinRegion
           , dhGetRegion = getBinRegion dh'
           }
      addStep $ DomainStep (Just (kernId k)) dh'
      return dh'

makeBinSubDomain :: Domain Bins -> Int -> Strategy (Domain Bins)
makeBinSubDomain _      nsplit | nsplit < 1 = fail "makeBinSubDomain: Split by 0?"
makeBinSubDomain parent nsplit = do
  did' <- freshDomainId
  let splitRegs _    _     [] = []
      splitRegs size start xs =
        let (pre, post) = span (\((l,_),_) -> l < start+size) xs
        in pre : splitRegs size (start+size) post
      region (BinRegion _ (Bins bins)) =
        let binsStart = fst $ fst $ Map.findMin bins
            binsEnd = snd $ fst $ Map.findMax bins
            splitBinSize = (binsEnd - binsStart) / fromIntegral nsplit
         in map (BinRegion dh' . Bins . Map.fromList) $
            splitRegs splitBinSize binsStart (Map.assocs bins)
      region _ = fail "makeBinSubDomain: Non-bin parent domain?"
      dh' :: Domain Bins
      dh' = Domain
        { dhId     = did'
        , dhSplit  = makeBinSubDomain dh'
        , dhCreate = const $ fail "dhCreate called on bin sub domain!"
        , dhRegion = return . region
        , dhParent = Just parent
        , dhRestrict = restrictBinRegion
        , dhFilterBox = filterBoxBin
        , dhPutRegion = putBinRegion
        , dhGetRegion = getBinRegion dh'
        }
  return dh'

unpackBinRegion :: Domain Bins -> RegionData -> IO Region
unpackBinRegion dh rdata = do

  -- The input might be split up by regions, unpack accordingly
  let mapTranspose = Map.unionsWith Map.union .
                     map (\(rbox, wmap) -> Map.map (Map.singleton rbox) wmap) .
                     Map.toList
  fmap (BinRegion dh . Bins . mapTranspose . Map.fromList) $ forM (Map.toList rdata) $ \(rbox, vec) -> do

    -- Vector is triples of (start, end, bin size), unpack
    -- accordingly. Note that we even derive the number of bins from
    -- the buffer size here - probably not the right way to do it?
    let vec' = castVector vec :: Vector Int64
        vecd' = castVector vec :: Vector Double
        bins = vectorSize vec' `div` 3
    binSizes <- forM [0..bins-1] $ \i ->
      (,) <$> ((,) <$> peekVector vecd' (i*3+0)
                   <*> peekVector vecd' (i*3+1))
          <*> (fromIntegral <$> peekVector vec' (i*3+2))

    -- Make regions.
    let regs = Map.filter (>0) $ Map.fromList binSizes
    return (rbox, regs)

restrictBinRegion :: Region -> [Region] -> [Region]
restrictBinRegion (BinRegion _ (Bins bins)) = mapMaybe restrict
  where restrict (BinRegion subDom (Bins subBins))
          | null filteredBins = Nothing
          | otherwise         = Just (BinRegion subDom (Bins filteredBins))
          where filteredBins = Map.filterWithKey (\k _ -> k `Map.member` bins) subBins
        restrict _other       = Nothing
restrictBinRegion other
  = error $ "restrictBinRegion: " ++ show other ++ " is no bin region!"

putBinRegion :: Region -> Put
putBinRegion (BinRegion _ (Bins bins)) = do
  put (Map.size bins)
  forM_ (Map.assocs bins) $ \((low,high), sizes) -> do
    put low
    put high
    put (Map.size sizes)
    forM_ (Map.assocs sizes) $ \(srbox, size) -> do
      putRegionBox srbox
      put size
putBinRegion other =
  fail $ "putBinRegion: " ++ show other ++ " is no bin region!"

getBinRegion :: Domain Bins -> GetContext -> Get Region
getBinRegion dom ctx = do
  numBins <- get
  bins <- replicateM numBins $ do
    low <- get
    high <- get
    numSizes <- get
    sizes <- replicateM numSizes ((,) <$> getRegionBox ctx <*> get)
    return ((low,high), Map.fromList sizes)
  return $ BinRegion dom (Bins (Map.fromList bins))

-- | Check whether a region box is permissable for the given bin
-- region. This is the case if all dependencies are fulfilled for any
-- box. Note that if a bin region has no dependencies, it is trivially
-- compatible with any region box.
filterBoxBin :: RegionBox -> Region -> Maybe Region
filterBoxBin rbox (BinRegion dom (Bins bins))
  | Map.null bins' = Nothing
  | otherwise      = Just $ BinRegion dom (Bins bins')
 where bins' = Map.filter (not . null) $ Map.map (Map.filterWithKey hasDeps) bins
       doms = map regionDomain rbox
       hasDeps deps _ = not $ any incompatible deps
       incompatible dep = (regionDomain dep `elem` doms) && not (dep `elem` rbox)
filterBoxBin other _
  = error $ "filterBoxBin: Expected bin region, but got " ++ show other ++ "!"

-- | Return the region of a region domain
regionRange :: Region -> (Int, Int)
regionRange (RangeRegion _ (Range low high))
               = (low, high)
regionRange _  = error "regionRange: Not a range region!"

data RegionBinDesc = RegionBinDesc
  { regionBinLow :: Double
  , regionBinHigh :: Double
  , regionBinSize :: Int
  }

-- | Return the bins in a bin domain
regionBins :: Region -> [RegionBinDesc]
regionBins (BinRegion _ (Bins bins))
             = let sumBin ((low, high), m) = RegionBinDesc low high (sum $ Map.elems m)
               in map sumBin (Map.toList bins)
regionBins _ = error "regionBins: Not a bin domain!"

-- | Split a 'Domain' up to produce a new 'Domain' with more 'Region's.
--
-- To be precise, for a given split factor @n@, we will attempt to
-- produce @n@ 'Region's in the returned 'Domain' for every 'Region'
-- of the input 'Domain'. So in the most basic case, using 'split' on
-- a primitive one-region 'Domain' will yield a new 'Domain'
-- containing @n@ 'Region's.
split :: Typeable a => Domain a -> Int -> Strategy (Domain a)
split dh parts = Strategy $ state $ \ss0 ->
  let (dh', ss1) = flip runState ss0 $ unStrategy $ dhSplit dh parts
      splitStep = DomainStep Nothing dh'
  in (dh', ss1{ ssSteps = splitStep : ssSteps ss1 })

-- | Perform computation in a distributed fashion. This means that the
-- given 'Strategy' will be executed separately for every 'Region' of
-- the 'Domain'. Depending on the 'Schedule', this can be used for
-- example to parallelise the work across nodes.
distribute :: Typeable a => Domain a -> Schedule -> Strategy () -> Strategy ()
distribute dh sched sub = Strategy $ modify $ \ ss0 ->
  let ((), ss1) = flip runState ss0{ ssSteps = [] } $ unStrategy sub
      splitStep = DistributeStep dh sched $ reverse $ ssSteps ss1
  in ss1{ ssSteps = splitStep : ssSteps ss0 }
