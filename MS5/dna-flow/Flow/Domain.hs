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
makeRangeDomain rhigh rlow = do
  d <- mkDom Nothing []
  addStep $ DomainStep d
  return d
 where region []     _ = Range rhigh rlow
       region (n:ns) i
         | Range low high <- region ns (i `div` n)
         = Range (low + (high-low) * (i `mod` n) `div` n)
                 (low + (high-low) * ((i `mod` n)+1) `div` n)
       mkDom parDh ns = do
         did' <- freshDomainId
         let dh' = DomainHandle { dhId = did'
                                , dhSize = product ns
                                , dhRegion = RangeDomain . region ns
                                , dhSplit = mkDom (Just dh') . (:ns)
                                , dhParent = parDh
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
