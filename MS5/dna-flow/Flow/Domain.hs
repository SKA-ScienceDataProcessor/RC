{-# LANGUAGE DeriveDataTypeable #-}

module Flow.Domain
  ( Domain
  , Schedule(..)
  , Range, makeRangeDomain
  , Bins, makeBinDomain, getBins
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
        }
  return dh'

unpackBinRegion :: Domain Bins -> Double -> Double -> Maybe (Vector ()) -> IO Region
unpackBinRegion _  _    _     Nothing =
  fail $ "unpackBinRegion: Expected buffer!"
unpackBinRegion dh rlow rhigh (Just vec) = do

  -- Vector is bin sizes, unpack accordingly. Note that we even derive
  -- the number of bins from the buffer size here - probably not the
  -- right way to do it.
  let vec' = castVector vec :: Vector Int32
      bins = vectorSize vec'
  binSizes <- unmakeVector vec' 0 bins

  -- Make regions
  let reg i size =
        ((rlow + fromIntegral i * (rhigh - rlow) / fromIntegral bins,
          rlow + fromIntegral (i+1) * (rhigh - rlow) / fromIntegral bins),
         fromIntegral size)
      regs = Map.fromList $ zipWith reg [(0::Int)..] binSizes
  return $ BinRegion dh (Bins regs)

-- | Return the bins in a bin domain
getBins :: Region -> [(Double, Double, Int)]
getBins (BinRegion _ (Bins bins))
          = map (\((low, high), size) -> (low, high, size)) $ Map.assocs bins
getBins _ = error "getBins: Not a bin domain!"

-- | Split a domain into sub-regions. This creates a new partitioned region, which
-- can be used to distribute both computation as well as data.
split :: Typeable a => Domain a -> Int -> (Domain a -> Strategy ()) -> Strategy ()
split dh parts sub = modify $ \ss0 ->
  let (dh', ss1) = flip runState ss0 (dhSplit dh parts)
      ((), ss2) = flip runState ss1{ ssSteps = [] } (sub dh')
      splitStep = SplitStep dh' $ reverse $ ssSteps ss2
  in ss2{ ssSteps = splitStep : ssSteps ss1 }

-- | Perform computation in a distributed fashion.
distribute :: Typeable a => Domain a -> Schedule -> Strategy () -> Strategy ()
distribute dh sched sub = modify $ \ ss0 ->
  let ((), ss1) = flip runState ss0{ ssSteps = [] } sub
      splitStep = DistributeStep dh sched $ reverse $ ssSteps ss1
  in ss1{ ssSteps = splitStep : ssSteps ss0 }
