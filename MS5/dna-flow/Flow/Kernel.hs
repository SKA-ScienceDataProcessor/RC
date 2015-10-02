{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Flow.Kernel
  ( DataRepr(..), ReprAccess(..)
  , NoRepr(..)
  , VectorRepr(..)
  , vecKernel0, vecKernel1, vecKernel2
  , HalideRepr(..), HalideFun
  , Dim1, dim1
  , halideKernel0, halideKernel1, halideKernel2
  , HalideKernel(..) -- newtype needs to be exported to prevent FFI error
  ) where

import Control.Applicative

import Data.Typeable

import Flow.Internal
import Flow.Builder
import Flow.Vector

import Flow.Halide.Marshal
import Flow.Halide.Types

-- | No representation: Either don't produce anything (= nobody can use
-- result) or don't care about input (= accept any input).
data NoRepr a = NoRepr
  deriving Typeable
instance Typeable a => Show (NoRepr a) where
  show _ = "nothing [" ++ show (typeOf (undefined :: a)) ++ "]"
instance Typeable a => DataRepr (NoRepr a) where
  type ReprType (NoRepr a) = a
  reprNop _ = True
  reprAccess _ = ReadAccess
  reprCompatible _ _ = True

-- | Vector representation: A variable-sized "Vector" with @val@
-- elements, representing abstract data of type @abs@. This is
-- essentially our low-level representation, so outside of the value
-- type check this will add no safety.
data VectorRepr val abs = VectorRepr ReprAccess
  deriving Typeable
instance (Typeable val, Typeable abs) => Show (VectorRepr val abs) where
  show _ = show (typeOf (undefined :: val)) ++ " vector [" ++ show (typeOf (undefined :: abs)) ++ "]"
instance (Typeable val, Typeable abs) => DataRepr (VectorRepr val abs) where
  type ReprType (VectorRepr val abs) = abs
  reprNop _ = False
  reprAccess (VectorRepr acc)  = acc
  reprCompatible _ _ = True

vecKernel0 :: (Typeable val, Typeable abs)
           => String -> VectorRepr val abs -> IO (Vector val) -> Kernel abs
vecKernel0 name rrepr code = kernel name Z rrepr $ \_ -> castVector <$> code

vecKernel1 :: (Typeable val, Typeable abs, Typeable val0, Typeable abs0)
           => String
           -> VectorRepr val0 abs0 -> VectorRepr val abs
           -> (Vector val0 -> IO (Vector val))
           -> Flow abs0 -> Kernel abs
vecKernel1 name repr0 rrepr code = kernel name (repr0 :. Z) rrepr $ \case
  [vec]  -> castVector <$> code (castVector vec)
  _other -> fail "vecKernel1: Received wrong number of input buffers!"

vecKernel2 :: (Typeable val, Typeable abs, Typeable val0, Typeable abs0, Typeable val1, Typeable abs1)
           => String
           -> VectorRepr val0 abs0 -> VectorRepr val1 abs1 -> VectorRepr val abs
           -> (Vector val0 -> Vector val1 -> IO (Vector val))
           -> Flow abs0 -> Flow abs1 -> Kernel abs
vecKernel2 name repr0 repr1 rrepr code = kernel name (repr0 :. repr1 :. Z) rrepr $ \case
  [vec,vec1] -> castVector <$> code (castVector vec) (castVector vec1)
  _other     -> fail "vecKernel2: Received wrong number of input buffers!"

-- | Halide array of statically known (!) size. Scalar type is @val@,
-- dimensionality is given by @dim@, and @abs@ identifies the abstract
-- data type.
data HalideRepr dim val abs = HalideRepr dim
  deriving Typeable
instance (Typeable dim, Typeable val, Typeable abs, HalideScalar val, Show dim) =>
         Show (HalideRepr dim val abs) where
  showsPrec _ (HalideRepr dim)
    = shows (typeOf (undefined :: val)) . showString " halide vector "
    . shows dim . showString " [" . shows (typeOf (undefined :: abs)) . showString "]"
instance (Typeable dim, Typeable val, Typeable abs, HalideScalar val, Show dim, Eq dim) =>
         DataRepr (HalideRepr dim val abs) where
  type ReprType (HalideRepr dim val abs) = abs
  reprNop _ = False
  reprAccess (HalideRepr _) = ReadAccess
  reprCompatible (HalideRepr d0) (HalideRepr d1) = d0 == d1

type HalideFun xs a = HalideKernel (KernelParams xs) (HalrParam a)
type family KernelParams (reprs :: [*]) :: [*]
type instance KernelParams '[] = '[]
type instance KernelParams (repr ': reprs) = HalrParam repr ': KernelParams reprs

-- We essentially "hide" HalideRepr in its own class here for the sole
-- reason of not having to repeat all its parameters and the full
-- context every time we use it.
--
-- Meanwhile, this also means we could potentially have more diverse
-- Halide data representations in future, plus possibly doing
-- newtype-tricks.
type HalrParam r = Array (HalrDim r) (HalrVal r)
class (DataRepr r, HalideScalar (HalrVal r), MarshalArray (HalrDim r)) =>
      HalideReprClass r where
  type HalrDim r
  type HalrVal r
  halrDim :: r -> HalrDim r
instance ( Typeable dim, MarshalArray dim, Show dim, Eq dim
         , Typeable val, HalideScalar val
         , Typeable abs
         ) =>
         HalideReprClass (HalideRepr dim val abs) where
  type HalrDim (HalideRepr dim val abs) = dim
  type HalrVal (HalideRepr dim val abs) = val
  halrDim (HalideRepr d) = d

halideKernel0 :: HalideReprClass rr
              => String
              -> rr
              -> HalideFun '[] rr
              -> Kernel (ReprType rr)
halideKernel0 name retR code = kernel name Z retR $ \_ -> do
  vecR <- call code (halrDim retR)
  return $ castVector $ arrayBuffer vecR

halideKernel1 :: (HalideReprClass rr, HalideReprClass r0)
              => String
              -> r0 -> rr
              -> HalideFun '[r0] rr
              -> Flow (ReprType r0) -> Kernel (ReprType rr)
halideKernel1 name rep0 repR code = kernel name (rep0 :. Z) repR code'
  where code' [v0] = do
         vecR <- call code (halrDim repR) (Array (halrDim rep0) (castVector v0))
         return $ castVector $ arrayBuffer vecR
        code' _other = fail "halideKernel1: Received wrong number of input buffers!"

halideKernel2 :: (HalideReprClass rr, HalideReprClass r0, HalideReprClass r1)
              => String
              -> r0 -> r1 -> rr
              -> HalideFun '[r0, r1] rr
              -> Flow (ReprType r0) -> Flow (ReprType r1) -> Kernel (ReprType rr)
halideKernel2 name rep0 rep1 repR code = kernel name (rep0 :. rep1 :. Z) repR code'
  where code' [v0, v1] = do
         vecR <- call code (halrDim repR) (Array (halrDim rep0) (castVector v0))
                                          (Array (halrDim rep1) (castVector v1))
         return $ castVector $ arrayBuffer vecR
        code' _other = fail "halideKernel2: Received wrong number of input buffers!"

