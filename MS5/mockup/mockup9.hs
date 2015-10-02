{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Typeable

import Flow
import Flow.Vector

import Foreign.C

import Data.Vector.HFixed.Class ()

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
type VecRepr = HalideRepr Dim1 Float Vec
vecRepr :: Int -> VecRepr
vecRepr size = HalideRepr ((0, fromIntegral size) :. Z)
type SumRepr = HalideRepr Z Float Sum
sumRepr :: SumRepr
sumRepr = HalideRepr Z

-- Kernels

fKern :: Int -> Kernel Vec
fKern size = halideKernel0 "f" (vecRepr size) kern_generate_f
foreign import ccall unsafe kern_generate_f :: HalideFun '[] VecRepr

gKern :: Int -> Kernel Vec
gKern size = halideKernel0 "g" (vecRepr size) kern_generate_g
foreign import ccall unsafe kern_generate_g :: HalideFun '[] VecRepr

ppKern :: Int -> Flow Vec -> Flow Vec -> Kernel Vec
ppKern size = halideKernel2 "pp" (vecRepr size) (vecRepr size) (vecRepr size)
                            kern_dotp
foreign import ccall unsafe kern_dotp :: HalideFun '[ VecRepr, VecRepr ] VecRepr

aKern :: Int -> Flow Vec -> Kernel Sum
aKern size = halideKernel1 "a" (vecRepr size) sumRepr kern_sum
foreign import ccall unsafe kern_sum :: HalideFun '[ VecRepr ] SumRepr

printKern :: Flow Sum -> Kernel Sum
printKern = kernel "print" code (sumRepr :. Z) sumRepr
  where code [sv] = do
          s <- peekVector (castVector sv :: Vector Float) 0
          putStrLn $ "Sum: " ++ show s
          return sv
        code _other = fail "printKern: Received wrong number of input buffers!"

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
main = do
  let size = 100000
  dumpSteps $ ddpStrat size
  execStrategy $ ddpStrat size
  putStrLn $ "Expected: " ++ show ((size-1)*size`div`20)
