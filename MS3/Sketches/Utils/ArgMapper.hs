{-# LANGUAGE
      TypeOperators
    , MultiParamTypeClasses
    , FlexibleInstances
    , FlexibleContexts
    , OverlappingInstances
    , UndecidableInstances
  #-}

module ArgMapper where

import Foreign.Storable

import CUDAEx

class FP a where
  funp :: a -> FunParam

instance FP Int where funp = IArg
instance FP Float where funp = FArg
instance FP Double where funp = DArg
instance Storable a => FP a where funp = VArg

class ArgMapper a where
  mapArgs :: a
  mapArgs = mapArgs0 []
  mapArgs0 :: [FunParam] -> a

instance ArgMapper [FunParam] where
  mapArgs0 acc = reverse acc

instance (FP a, ArgMapper r) => ArgMapper (a -> r) where
  mapArgs0 acc v = mapArgs0 (funp v : acc)
