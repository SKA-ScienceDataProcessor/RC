{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Typeable

import Strategy.Builder
import Strategy.Domain
import Strategy.Exec
import Strategy.Vector
import Strategy.Kernel

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
vecRepr :: VectorRepr Double Vec
vecRepr = VectorRepr ReadAccess
sumRepr :: VectorRepr Double Sum
sumRepr = VectorRepr ReadAccess

-- Kernels
fKern :: Int -> Kernel Vec
fKern size = vecKernel0 "f" vecRepr $ do
  v <- allocCVector size
  forM_ [1..size] $ \i ->
    pokeVector v i (fromIntegral i)
  return v
gKern :: Int -> Kernel Vec
gKern size = vecKernel0 "g" vecRepr $ do
  v <- allocCVector size
  forM_ [1..size] $ \i ->
    pokeVector v i 2.0
  return v

ppKern :: Int -> Flow Vec -> Flow Vec -> Kernel Vec
ppKern size = vecKernel2 "pp" vecRepr vecRepr vecRepr $ \fv gv -> do
  v <- allocCVector size
  forM_ [1..size] $ \i -> do
    a <- peekVector fv i
    b <- peekVector gv i
    pokeVector v i (a * b)
  return v

aKern :: Int -> Flow Vec -> Kernel Sum
aKern size = vecKernel1 "sum" vecRepr sumRepr $ \iv -> do
  v <- allocCVector 1
  s <- flip (flip foldM 0) [1..size] $ \s i -> do
    a <- peekVector iv i
    return (s + a)
  pokeVector v 0 s
  return v

printKern :: Flow Sum -> Kernel Sum
printKern = vecKernel1 "print" sumRepr sumRepr $ \sv -> do
  s <- peekVector sv 0
  putStrLn $ "Sum: " ++ show s
  return sv

ddpStrat :: Int -> Strategy ()
ddpStrat size = do

  -- Make vector domain
  dom <- makeRangeDomain $ Range 0 (size-1)

  -- Calculate ddp for the whole domain
  bind1D dom f (fKern size)
  bind1D dom g (gKern size)
  bindRule1D dom pp (ppKern size)
  bindRule1D dom a (aKern size)
  calculate ddp
  rebind1D dom ddp printKern

main :: IO ()
main = execStrategy $ ddpStrat 1000
