{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Strategy.Builder
import Strategy.Domain
import Strategy.Data
import Strategy.Exec

import Data.Typeable

import qualified Data.ByteString as BS

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
fKern = kernel "f" code HNil vecRepr
  where code _ = putStrLn "f" >> return BS.empty
gKern :: Kernel Vec
gKern = kernel "g" code HNil vecRepr
  where code _ = putStrLn "g" >> return BS.empty
ppKern :: Flow Vec -> Flow Vec -> Kernel Vec
ppKern = kernel "pp" code (vecRepr :. vecRepr :. HNil) vecRepr
  where code _ = putStrLn "pp" >> return BS.empty
aKern :: Flow Vec -> Kernel Sum
aKern = kernel "a" code (vecRepr :. HNil) sumRepr
  where code _ = putStrLn "a" >> return BS.empty

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
main = execStrategy $ ddpStrat 1000
