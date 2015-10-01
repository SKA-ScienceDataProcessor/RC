{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Strategy.Kernel
  ( module Strategy.Vector
  , VectorRepr(..)
  , vecKernel0, vecKernel1, vecKernel2
  , HalideRepr(..)
  , halideKernel0, halideKernel1, halideKernel2
  ) where

import Control.Applicative

import Data.Typeable

import Strategy.Internal
import Strategy.Builder
import Strategy.Vector

import Halide.Marshal
import qualified Halide.Types as Halide

data VectorRepr val abs = VectorRepr ReprAccess
  deriving Typeable

instance (Typeable val, Typeable abs) => Show (VectorRepr val abs) where
  show _ = show (typeOf (undefined :: val)) ++ " vector [" ++ show (typeOf (undefined :: abs)) ++ "]"
instance (Typeable val, Typeable abs) => DataRepr (VectorRepr val abs) where
  type RPar (VectorRepr val abs) = abs
  reprNop _ = False
  reprAccess (VectorRepr acc)  = acc
  reprCompatible _ _ = True

vecKernel0 :: (Typeable val, Typeable abs)
           => String -> VectorRepr val abs -> IO (Vector val) -> Kernel abs
vecKernel0 name rrepr code = kernel name code' HNil rrepr
  where code' _ = castVector <$> code

vecKernel1 :: (Typeable val, Typeable abs, Typeable val0, Typeable abs0)
           => String
           -> VectorRepr val0 abs0 -> VectorRepr val abs
           -> (Vector val0 -> IO (Vector val))
           -> Flow abs0 -> Kernel abs
vecKernel1 name repr0 rrepr code = kernel name code' (repr0 :. HNil) rrepr
  where code' [vec] = castVector <$> code (castVector vec)
        code' _other = fail "vecKernel1: Received wrong number of input buffers!"

vecKernel2 :: (Typeable val, Typeable abs, Typeable val0, Typeable abs0, Typeable val1, Typeable abs1)
           => String
           -> VectorRepr val0 abs0 -> VectorRepr val1 abs1 -> VectorRepr val abs
           -> (Vector val0 -> Vector val1 -> IO (Vector val))
           -> Flow abs0 -> Flow abs1 -> Kernel abs
vecKernel2 name repr0 repr1 rrepr code = kernel name code' (repr0 :. repr1 :. HNil) rrepr
  where code' [vec,vec1] = castVector <$> code (castVector vec) (castVector vec1)
        code' _other = fail "vecKernel2: Received wrong number of input buffers!"

-- | Halide array of statically known (!) size
data HalideRepr dim val abs = HalideRepr { halrDim :: dim }
  deriving Typeable

instance (Typeable dim, Typeable val, Typeable abs, HalideScalar val, Show dim) =>
         Show (HalideRepr dim val abs) where
  showsPrec _ (HalideRepr dim)
    = shows (typeOf (undefined :: val)) . showString " halide vector "
    . shows dim . showString " [" . shows (typeOf (undefined :: abs)) . showString "]"
instance (Typeable dim, Typeable val, Typeable abs, HalideScalar val, Show dim, Eq dim) =>
         DataRepr (HalideRepr dim val abs) where
  type RPar (HalideRepr dim val abs) = abs
  reprNop _ = False
  reprAccess (HalideRepr _) = ReadAccess
  reprCompatible (HalideRepr dim0) (HalideRepr dim1) = dim0 == dim1

halideKernel0 :: (Typeable dim, Typeable val, Typeable abs, HalideScalar val, MarshalArray dim, Show dim, Eq dim)
              => String
              -> HalideRepr dim val abs
              -> Halide.Kernel '[] (Halide.Array dim val)
              -> Kernel abs
halideKernel0 name retR code = kernel name code' HNil retR
  where code' _ = do
          vecR <- call code (halrDim retR)
          return $ castVector $ Halide.arrayBuffer vecR

halideKernel1 :: ( Typeable dim, Typeable val, Typeable abs, HalideScalar val, MarshalArray dim, Show dim, Eq dim
                 , Typeable dim0, Typeable val0, Typeable abs0, HalideScalar val0, MarshalArray dim0, Show dim0, Eq dim0)
              => String
              -> HalideRepr dim0 val0 abs0 -> HalideRepr dim val abs
              -> Halide.Kernel '[Halide.Array dim0 val0] (Halide.Array dim val)
              -> Flow abs0 -> Kernel abs
halideKernel1 name rep0 repR code = kernel name code' (rep0 :. HNil) repR
  where code' [v0] = do
         vecR <- call code (halrDim repR) (Halide.Array (halrDim rep0) (castVector v0))
         return $ castVector $ Halide.arrayBuffer vecR
        code' _other = fail "halideKernel1: Received wrong number of input buffers!"

halideKernel2 :: ( Typeable dim, Typeable val, Typeable abs, HalideScalar val, MarshalArray dim, Show dim, Eq dim
                 , Typeable dim0, Typeable val0, Typeable abs0, HalideScalar val0, MarshalArray dim0, Show dim0, Eq dim0
                 , Typeable dim1, Typeable val1, Typeable abs1, HalideScalar val1, MarshalArray dim1, Show dim1, Eq dim1)
              => String
              -> HalideRepr dim0 val0 abs0 -> HalideRepr dim1 val1 abs1 -> HalideRepr dim val abs
              -> Halide.Kernel '[Halide.Array dim0 val0, Halide.Array dim1 val1] (Halide.Array dim val)
              -> Flow abs0 -> Flow abs1 -> Kernel abs
halideKernel2 name rep0 rep1 repR code = kernel name code' (rep0 :. rep1 :. HNil) repR
  where code' [v0, v1] = do
         vecR <- call code (halrDim repR) (Halide.Array (halrDim rep0) (castVector v0))
                                          (Halide.Array (halrDim rep1) (castVector v1))
         return $ castVector $ Halide.arrayBuffer vecR
        code' _other = fail "halideKernel2: Received wrong number of input buffers!"
