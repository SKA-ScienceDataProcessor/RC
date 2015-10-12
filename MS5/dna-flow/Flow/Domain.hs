{-# LANGUAGE DeriveDataTypeable #-}

module Flow.Domain
  ( DomainHandle
  , Schedule(..)
  , Range, makeRangeDomain
  , split, distribute
  ) where

import Control.Monad.State.Strict

import Data.Typeable

import Flow.Internal

-- | Create a new range domain
makeRangeDomain :: Int -> Int -> Strategy (DomainHandle Range)
makeRangeDomain rlow rhigh = do
  d <- makeRangeDomain' rlow rhigh 1 Nothing 1
  addStep $ DomainStep d
  return d

makeRangeDomain' :: Int -> Int -> Int -> Maybe (DomainHandle Range) -> Int -> Strategy (DomainHandle Range)
makeRangeDomain' rlow rhigh size parDh nsplit = do
  did' <- freshDomainId
  let region (RangeDomain (Range low high)) i
        = Range (low + (high-low) * i `div` nsplit)
                (low + (high-low) * (i+1) `div` nsplit)
  let dh' = DomainHandle
       { dhId     = did'
       , dhSize   = size*nsplit
       , dhSplit  = makeRangeDomain' rlow rhigh (size*nsplit) (Just dh')
       , dhParent = parDh
       , dhCreate = return $ RangeDomain (Range rlow rhigh)
       , dhRegion = \d -> return $ map (RangeDomain . region d) [0..nsplit-1]
       }
  return dh'

-- | Split a domain into sub-regions. This creates a new partitioned region, which
-- can be used to distribute both computation as well as data.
split :: Typeable a => DomainHandle a -> Int -> (DomainHandle a -> Strategy ()) -> Strategy ()
split dh parts sub = modify $ \ss0 ->
  let (dh', ss1) = flip runState ss0 (dhSplit dh parts)
      ((), ss2) = flip runState ss1{ ssSteps = [] } (sub dh')
      splitStep = SplitStep dh' $ reverse $ ssSteps ss2
  in ss2{ ssSteps = splitStep : ssSteps ss1 }

-- | Perform computation in a distributed fashion.
distribute :: Typeable a => DomainHandle a -> Schedule -> Strategy () -> Strategy ()
distribute dh sched sub = modify $ \ ss0 ->
  let ((), ss1) = flip runState ss0{ ssSteps = [] } sub
      splitStep = DistributeStep dh sched $ reverse $ ssSteps ss1
  in ss1{ ssSteps = splitStep : ssSteps ss0 }
