{-# LANGUAGE DeriveDataTypeable #-}

module Flow.Domain
  ( Domain, Region, RegionBox
  , Schedule(..)
  , Range, makeRangeDomain, regionRange
  , Bins, makeBinDomain, regionBins
  , split, distribute
  ) where

import Control.Monad.State.Strict

import Data.Int
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Typeable

import Flow.Internal
import Flow.Builder
import Flow.Vector

-- | Create a new range domain
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
       , dhFilterBox = \_ -> Just -- no dependencies
       }
  return dh'

-- | Create a new bin domain
makeBinDomain :: Kernel a -> Double -> Double -> Strategy (Domain Bins)
makeBinDomain kern rlow rhigh = do

  -- Bind an internal kernel flow
  Flow dflow <- bindNew kern
  ss <- get
  case HM.lookup dflow (ssMap ss) of
    Nothing -> fail $ "makeBinDomain: Internal error - failed to look up flow " ++ show dflow ++ "!"
    Just k -> do
      did' <- freshDomainId
      let dh' :: Domain Bins
          dh' = Domain
           { dhId     = did'
           , dhSplit  = makeBinSubDomain dh'
           , dhParent = Nothing
           , dhCreate = unpackBinRegion dh' rlow rhigh
           , dhRegion = fail "dhRegion called on bin root domain!"
           , dhFilterBox = filterBoxBin
           }
      addStep $ DomainStep (Just (kernId k)) dh'
      return dh'

makeBinSubDomain :: Domain Bins -> Int -> Strategy (Domain Bins)
makeBinSubDomain _      nsplit | nsplit < 1 = fail "makeBinSubDomain: Split by 0?"
makeBinSubDomain parent nsplit = do
  did' <- freshDomainId
  let splitRegs 1 _ xs = [xs]
      splitRegs i n xs = start : splitRegs (i-1) (n - n `div` i) rest
        where (start,rest) = splitAt (n `div` i) xs
      region (BinRegion _ (Bins bins)) = map (BinRegion dh' . Bins . Map.fromList) $
        splitRegs nsplit (Map.size bins) (Map.assocs bins)
      region _ = fail "makeBinSubDomain: Non-bin parent domain?"
      dh' :: Domain Bins
      dh' = Domain
        { dhId     = did'
        , dhSplit  = makeBinSubDomain dh'
        , dhCreate = const $ fail "dhCreate called on bin sub domain!"
        , dhRegion = return . region
        , dhParent = Just parent
        , dhFilterBox = filterBoxBin
        }
  return dh'

unpackBinRegion :: Domain Bins -> Double -> Double -> RegionData -> IO Region
unpackBinRegion dh rlow rhigh rdata = do

  -- The input might be split up by regions, unpack accordingly
  let mapTranspose = Map.unionsWith Map.union .
                     map (\(rbox, wmap) -> Map.map (Map.singleton rbox) wmap) .
                     Map.toList
  fmap (BinRegion dh . Bins . mapTranspose . Map.fromList) $ forM (Map.toList rdata) $ \(rbox, vec) -> do

    -- Vector is bin sizes, unpack accordingly. Note that we even derive
    -- the number of bins from the buffer size here - probably not the
    -- right way to do it.
    let vec' = castVector vec :: Vector Int32
        bins = vectorSize vec'
    binSizes <- unmakeVector vec' 0 bins

    -- Make regions. This would all be nicer if this was simply part of
    -- the vector. TODO...
    let reg i size =
          ((rlow + fromIntegral i * (rhigh - rlow) / fromIntegral bins,
            rlow + fromIntegral (i+1) * (rhigh - rlow) / fromIntegral bins),
           fromIntegral size)
        regs = Map.filter (>0) $ Map.fromList $ zipWith reg [(0::Int)..] binSizes
    return (rbox, regs)

-- | Check whether a region box is permissable for the given bin
-- region. This is the case if all dependencies are fulfilled for any
-- box. Note that if a bin region has no dependencies, it is trivially
-- compatible with any region box.
filterBoxBin :: RegionBox -> Region -> Maybe Region
filterBoxBin rbox (BinRegion dom (Bins bins))
  | Map.null (Map.filter (not . Map.null) bins')
               = Nothing
  | otherwise  = Just $ BinRegion dom (Bins bins')
 where bins' = Map.map (Map.filterWithKey hasDeps) bins
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

-- | Return the bins in a bin domain
regionBins :: Region -> [(Double, Double, Int)]
regionBins (BinRegion _ (Bins bins))
             = let sumBin ((low, high), m) = (low, high, sum $ Map.elems m)
               in map sumBin (Map.toList bins)
regionBins _ = error "regionBins: Not a bin domain!"

-- | Split a domain into sub-regions. This creates a new partitioned region, which
-- can be used to distribute both computation as well as data.
split :: Typeable a => Domain a -> Int -> Strategy (Domain a)
split dh parts = state $ \ss0 ->
  let (dh', ss1) = flip runState ss0 (dhSplit dh parts)
      splitStep = DomainStep Nothing dh'
  in (dh', ss1{ ssSteps = splitStep : ssSteps ss1 })

-- | Perform computation in a distributed fashion.
distribute :: Typeable a => Domain a -> Schedule -> Strategy () -> Strategy ()
distribute dh sched sub = modify $ \ ss0 ->
  let ((), ss1) = flip runState ss0{ ssSteps = [] } sub
      splitStep = DistributeStep dh sched $ reverse $ ssSteps ss1
  in ss1{ ssSteps = splitStep : ssSteps ss0 }
