{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Typeable

import Strategy.Builder
import Strategy.Domain
import Strategy.Exec
import Strategy.Vector
import Strategy.Kernel

import qualified Halide.Types as Halide

import Foreign.C

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
vecRepr :: Int -> HalideRepr Halide.Dim1 Float Vec
vecRepr size = HalideRepr ((0, fromIntegral size) Halide.:. Halide.Z)
sumRepr :: HalideRepr Halide.Dim1 Float Sum
sumRepr = HalideRepr ((0,1) Halide.:. Halide.Z)

-- Kernels

fKern :: Int -> Kernel Vec
fKern size = halideKernel0 "f" (vecRepr size) kern_generate_f
foreign import ccall "kern_generate_f"
  kern_generate_f :: Halide.Kernel '[] (Halide.Array Halide.Dim1 Float)

gKern :: Int -> Kernel Vec
gKern size = halideKernel0 "g" (vecRepr size) kern_generate_g
foreign import ccall "kern_generate_g"
  kern_generate_g :: Halide.Kernel '[] (Halide.Array Halide.Dim1 Float)

ppKern :: Int -> Flow Vec -> Flow Vec -> Kernel Vec
ppKern size = halideKernel2 "pp" (vecRepr size) (vecRepr size) (vecRepr size)
                            kern_dotp
foreign import ccall "kern_dotp"
  kern_dotp :: Halide.Kernel '[ Halide.Array Halide.Dim1 Float
                              , Halide.Array Halide.Dim1 Float] (Halide.Array Halide.Dim1 Float)

aKern :: Int -> Flow Vec -> Kernel Sum
aKern size = halideKernel1 "a" (vecRepr size) sumRepr kern_sum
foreign import ccall "kern_sum"
  kern_sum :: Halide.Kernel '[Halide.Array Halide.Dim1 Float] (Halide.Array Halide.Dim1 Float)

printKern :: Flow Sum -> Kernel Sum
printKern = kernel "print" code (sumRepr :. HNil) sumRepr
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
  let size = 1000
  execStrategy $ ddpStrat size
  putStrLn $ "Expected: " ++ show ((size-1)*size`div`20)
