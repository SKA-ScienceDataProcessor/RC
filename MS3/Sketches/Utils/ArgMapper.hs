{-# LANGUAGE
      TypeOperators
    , FlexibleInstances
    , OverlappingInstances
  #-}

module ArgMapper where

import Foreign.Storable

import CUDAEx

infixr 5 :.

data Z = Z
data a :. b = a :. b

class ArgMapper a where
  mapArgs :: a -> [FunParam]

instance ArgMapper Z where mapArgs _ = []

instance ArgMapper ts => ArgMapper (Int :. ts) where
  mapArgs (v :. vs) = (IArg v) : mapArgs vs

instance ArgMapper ts => ArgMapper (Float :. ts) where
  mapArgs (v :. vs) = (FArg v) : mapArgs vs

instance ArgMapper ts => ArgMapper (Double :. ts) where
  mapArgs (v :. vs) = (DArg v) : mapArgs vs

instance (Storable t, ArgMapper ts) => ArgMapper (t :. ts) where
  mapArgs (v :. vs) = (VArg v) : mapArgs vs


infixr 0 |<

(|<) :: ArgMapper a => ([FunParam] -> b) -> a -> b
(|<) f args = f (mapArgs args)
