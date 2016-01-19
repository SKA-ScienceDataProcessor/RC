{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Typeable
import Data.Int
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flow
import Flow.Vector
import Flow.Halide

-- Needed for FFI to work
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

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
type VecRepr = DynHalideRepr Dim0 Float Vec
vecRepr :: Domain Range -> VecRepr
vecRepr = dynHalideRepr dim0
type SumRepr = HalideRepr Z Float Sum
sumRepr :: SumRepr
sumRepr = halideRepr Z

-- Kernels

fKern :: Domain Range -> Kernel Vec
fKern size = halideKernel0 "f" (vecRepr size) kern_generate_f''

foreign import ccall unsafe kern_generate_f :: HalideFun '[] VecRepr

kern_generate_f' :: HalideFun '[] VecRepr
kern_generate_f' = case kern_generate_f of
  HalideKernel ff -> HalideKernel $ \p -> do
    n <- peekByteOff (castPtr p) 48
    case n :: Int32 of
      0 -> error "Zero arrived!"
      _ -> ff p

kern_generate_f'' :: HalideFun '[] VecRepr
kern_generate_f'' = HalideKernel $ error "I crash always!"

gKern :: Domain Range -> Kernel Vec
gKern size = halideKernel0 "g" (vecRepr size) kern_generate_g
foreign import ccall unsafe kern_generate_g :: HalideFun '[] VecRepr

ppKern :: Domain Range -> Flow Vec -> Flow Vec -> Kernel Vec
ppKern size = halideKernel1Write "pp" (vecRepr size) (vecRepr size) kern_dotp
foreign import ccall unsafe kern_dotp :: HalideFun '[ VecRepr ] VecRepr

aKern :: Domain Range -> Flow Vec -> Kernel Sum
aKern size = halideKernel1 "a" (vecRepr size) sumRepr kern_sum
foreign import ccall unsafe kern_sum :: HalideFun '[ VecRepr ] SumRepr

-- Dummy recover kernel
recoverKern :: Domain Range -> Kernel Vec
recoverKern size = halideKernel0 "recover" (vecRepr size) kern_recovery
foreign import ccall unsafe kern_recovery :: HalideFun '[] VecRepr


printKern :: Flow Sum -> Kernel Sum
printKern = mergingKernel "print" (sumRepr :. Z) NoRepr $ \case
  [(sv,_)]-> \_ -> do
    s <- peekVector (castVector sv :: Vector Float) 0
    putStrLn $ "Sum: " ++ show s
    return nullVector
  _other -> fail "printKern: Received wrong number of input buffers!"

-- | Dot product, non-distributed
dpStrat :: Int -> Strategy ()
dpStrat size = do

  -- Make vector domain
  dom <- makeRangeDomain 0 size

  -- Calculate ddp for the whole domain
  bind f (fKern dom)
  bind g (gKern dom)
  recover f (recoverKern dom)
  recover g (recoverKern dom)
  
  bindRule pp (ppKern dom)
  bindRule a (aKern dom)
  calculate ddp
  rebind ddp printKern

-- | Dot product, distributed
ddpStrat :: Int -> Strategy ()
ddpStrat size = do

  -- Make vector domain
  dom <- makeRangeDomain 0 size

  -- Calculate ddp for the whole domain
  regs <- split dom 3
  distribute regs ParSchedule $ do
    bind f (fKern regs)
    bind g (gKern regs)
    bind (pp f g) (ppKern regs f g)
  --
  recover f  (recoverKern regs)
  recover g  (recoverKern regs)
  recover (pp f g) (recoverKern regs)
  bindRule a (aKern dom)
  calculate ddp
  void $ bindNew $ printKern ddp

main :: IO ()
main = do
  let size = 1000000
  dumpSteps $ ddpStrat size
  print "================================================================"
  execStrategyDNA $ ddpStrat size
  putStrLn $ "Expected: " ++ show ((size-1)*size`div`20)
