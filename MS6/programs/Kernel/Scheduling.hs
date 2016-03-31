
module Kernel.Scheduling where

import Control.Monad

import Data.Function ( on )
import Data.Ratio
import Data.Int
import Data.List  ( findIndex, minimumBy )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )

import Kernel.Data

import Flow
import Flow.Domain
import Flow.Kernel
import Flow.Vector

makeOskarDomain :: Config -> Int -> Strategy (Domain Bins, Flow Index)
makeOskarDomain cfg = makeScheduleDomain (cfgInput cfg) oskarRepeat oskarWeight

-- | Given a set of entities to schedule with associated repeats and weights,
-- make a new 'Bin' domain with associated index 'Flow'.
makeScheduleDomain
  :: [a]              -- ^ Items to schedule
  -> (a -> Int)       -- ^ Number of repeats for item
  -> (a -> Double)    -- ^ Weight of item
  -> Int              -- ^ Number of nodes (= split to prepare)
  -> Strategy (Domain Bins, Flow Index)
makeScheduleDomain items repeats weight nodes = do

  -- Balance
  let totalWeight a = fromIntegral (repeats a) * weight a
      balance = balancer nodes (map totalWeight items)

  -- We will now make a bin domain where every repeat is going to be
  -- one bin, with the size of the bin given by the faction of nodes
  -- we allocate to it. This is so that when we split the bins by the
  -- number of nodes later, we get exactly what we'd expect.
  let repBins :: [(Ratio Int, Int32)]
      repBins = concat $ zipWith3 repBin items balance [0..]
      repBin item alloc i = replicate (repeats item) (alloc % repeats item, i)
      repBinStarts = scanl (+) 0 $ map fst repBins
      totalReps = length repBins

  -- Make bin domain from a vector describing the bins as outlined
  -- above.
  let binRepr = VectorRepr WriteAccess :: VectorRepr Int ()
  ddom <- makeBinDomain $ mappingKernel "scheduler" Z binRepr $ \_ _ -> do
    binVec <- allocCVector (3 * totalReps) :: IO (Vector Int64)
    let binVecDbl = castVector binVec :: Vector Double
    forM_ (zip3 [0..] repBins repBinStarts) $ \(n, (size, _), start) -> do
      pokeVector binVecDbl (n*3+0) (fromRational $ toRational start)
      pokeVector binVecDbl (n*3+1) (fromRational $ toRational (start+size))
      pokeVector binVec    (n*3+2) 1
    return (castVector binVec)

  -- Make a Flow containing the indices
  ixFlow <- bindNew $ mappingKernel "indices" Z (indexRepr ddom) $ \_ _ ->
    castVector <$> (makeVector allocCVector $ map snd repBins)

  -- Finally, define a kernel that can be used to split the index set
  -- All done
  return (ddom, ixFlow)

-- | Fairly simple kernel useful for splitting schedules as produced
-- by 'makeScheduleDomain'.
scheduleSplit :: DDom -> DDom -> Flow Index -> Kernel Index
scheduleSplit inDom outDom = mappingKernel "schedule splitter" (indexRepr inDom :. Z) (indexRepr outDom) $
  \[ixPar] outRBox -> do

    -- Get input and output bins
    let ((inRBox, ixs):_) = Map.assocs ixPar
        inBins = regionBins $ head inRBox
        outBins = regionBins $ head outRBox

    -- Check how many bins we need to drop
    let isOutStart bin = regionBinLow bin == regionBinLow (head outBins)
        toDrop = fromMaybe (error "Could not find start output bin in input bins!")
                           (findIndex isOutStart inBins)

    -- Make new vector for the output region
    inIxs <- unmakeVector (castVector ixs) 0 (length inBins) :: IO [Int32]
    castVector <$> (makeVector allocCVector $ take (length outBins)
                                            $ drop toDrop inIxs)


balancer
    :: Int      -- ^ Number of nodes
    -> [Double] -- ^ Full execution times for each freq. channel (T for image × N iteraterion)
    -> [Int]    -- ^ Number of nodes allocated to each channel
balancer n ts = snd $ minimumBy (compare `on` fst) $ do
    -- Generate all possible schedules
    nTail <- replicateM (nCh - 1) [1 .. n - nCh + 1]
    let n0 = n - sum nTail
        ns = n0 : nTail
    guard (n0 > 0)
    -- Return pair of execution time and schedule
    return (maximum $ zipWith (\k t -> t / fromIntegral k) ns ts, ns)
  where
    nCh = length ts
