{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Strategy.Builder
import Strategy.Domain
import Strategy.Data
import Strategy.Dump

import Data.Typeable

-- Data tags
data Vec deriving Typeable
data Sum deriving Typeable

-- Abstract flow signatures
f, g :: Flow Vec
f = flow "f"
g = flow "g"
pp :: Flow Vec -> Flow Vec -> Flow Vec
pp = flow "product"
a :: Flow Vec -> Flow Sum
a = flow "sum"
ddp :: Flow Sum
ddp = a $ pp f g

-- Vector representation
vecRepr :: CBufRepr Vec
vecRepr = CBufRepr ReadAccess
sumRepr :: CBufRepr Sum
sumRepr = CBufRepr ReadAccess

-- Kernels
fKern :: Kernel Vec
fKern = kernel "f" HNil vecRepr
gKern :: Kernel Vec
gKern = kernel "g" HNil vecRepr
ppKern :: Flow Vec -> Flow Vec -> Kernel Vec
ppKern = kernel "pp" (vecRepr :. vecRepr :. HNil) vecRepr
aKern :: Flow Vec -> Kernel Sum
aKern = kernel "a" (vecRepr :. HNil) sumRepr

ddpStrat :: Int -> Strategy ()
ddpStrat size = do

  -- Make vector domain
  dom <- makeRangeDomain $ Range 0 (size-1)

  -- Calculate ddp for the whole domain
  bind1D dom f fKern
  bind1D dom g gKern
  bind1D dom (pp f g) (ppKern f g)
  bind1D dom ddp (aKern (pp f g))

main :: IO ()
main = dumpSteps $ ddpStrat 1000
