{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}

module Main where

import Data.Typeable

import Control.Arrow (Arrow(..))
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Monoid (Monoid(..))
import Flow
import Flow.Vector
import Flow.Halide
import Text.PrettyPrint
-- import DNA (dnaRun)

-- Needed for FFI to work
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import Data.Vector.HFixed.Class ()
import qualified Data.Foldable as T
import Flow.Halide.Types ()
import Flow.DnaCompiler
import Flow.Internal
import Bound

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
fKern size = halideKernel0 "f" (vecRepr size) kern_generate_f
foreign import ccall unsafe kern_generate_f :: HalideFun '[] VecRepr

gKern :: Domain Range -> Kernel Vec
gKern size = halideKernel0 "g" (vecRepr size) kern_generate_g
foreign import ccall unsafe kern_generate_g :: HalideFun '[] VecRepr

ppKern :: Domain Range -> Flow Vec -> Flow Vec -> Kernel Vec
ppKern size = halideKernel1Write "pp" (vecRepr size) (vecRepr size) kern_dotp
foreign import ccall unsafe kern_dotp :: HalideFun '[ VecRepr ] VecRepr

aKern :: Domain Range -> Flow Vec -> Kernel Sum
aKern size = halideKernel1 "a" (vecRepr size) sumRepr kern_sum
foreign import ccall unsafe kern_sum :: HalideFun '[ VecRepr ] SumRepr

printKern :: Flow Sum -> Kernel Sum
printKern = kernel "print" (sumRepr :. Z) NoRepr $ \case
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
  split dom 10 $ \regs ->
    distribute regs ParSchedule $ do
      bind f (fKern regs)
      bind g (gKern regs)
      bind (pp f g) (ppKern regs f g)
  bindRule a (aKern dom)
  calculate ddp
  void $ bindNew $ printKern ddp


dumpES ind (Step s) = dumpStep ind s
dumpES ind (Call dh pars i) = do
  putStrLn $ ind ++ "Call " ++ show i ++ " " ++ show pars
  dumpStep (ind ++ "  ") $ SplitStep dh []
-- dumpES ind (SplitDistr dh' sched ss) = do
--   dumpStep ind (DistributeStep dh' sched [])
--   mapM_ (dumpES (ind++"  ")) ss
dumpES ind (Expect vs) =
  putStrLn $ ind ++ "Expect " ++ show vs
dumpES ind (Yield vs) =
  putStrLn $ ind ++ "Yield " ++ show vs
dumpES ind (Gather vs) =
  putStrLn $ ind ++ "Gather " ++ show vs


dumpTreeWith :: (String -> a -> IO ()) -> ActorTree a -> IO ()
dumpTreeWith out = go ""
  where
    go off (ActorTree a m) = do
      out off a
      let off' = "  " ++ off
      forM_ (HM.toList m) $ \(i,a') -> do
        putStrLn $ off' ++ "[Key=" ++ show i ++ "]"
        go off' a'

dumpTree :: Show a => ActorTree a -> IO ()
dumpTree = dumpTreeWith (\off a -> putStr off >> print a)
  

main :: IO ()
main = do
  -- Peter's part
  let size  = 1000000
      -- strat = dpStrat size
      strat = ddpStrat size
      steps = runStrategy strat
  putStrLn "----------------------------------------------------------------"
  dumpSteps strat
  -- Transformation
  let ast0@(ActorTree ss acts) = makeActorTree steps
      ast1 = findInOut ast0
      ast2 = addCommands ast1
  putStrLn "\n-- Actor split -------------------------------------------------"
  dumpTreeWith (\off -> mapM_ (dumpES off) . snd) ast0
  putStrLn "\n-- Used --------------------------------------------------------"
  dumpTree $ fmap (varsUsed . fst . snd) ast1
  putStrLn "-- Produced ----------------------------------------------------"
  dumpTree $ fmap (varsProd . fst . snd) ast1
  putStrLn "-- Missing -----------------------------------------------------"
  dumpTree $ fmap (varsMissing . fst . snd) ast1
  --
  putStrLn "\n-- Annotated ---------------------------------------------------"  
  dumpTreeWith (\off -> mapM_ (dumpES (off++"")) . snd . snd) ast2
  -- Find parameters and return values
  -- let walk x f (ActorTree v hm)
  --       = ActorTree x (fmap (go v) hm)
  --       where
  --         go a f (ActorTree vv hmm) = ActorTree (f a vv) (fmap (go vv) hmm)
  -- return ()
  -- putStrLn "-- Param - -----------------------------------------------------"
  -- let getp vPar vCh = ( varsProd vPar `HS.intersection` varsMissing vCh
  --                     , varsMissing vPar `HS.intersection` varsProd vCh
  --                     )  
  -- dumpTree $ walkTree (mempty,mempty) getp vars
  -- Find out what to pass as parameters

