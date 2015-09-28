{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Strategy.Kernel
  ( module Strategy.Vector
  , VectorRepr(..)
  , vecKernel0, vecKernel1, vecKernel2
  ) where

import Control.Applicative

import Data.Typeable

import Strategy.Internal
import Strategy.Builder
import Strategy.Vector

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

vecKernel2 :: (Typeable val, Typeable abs, Typeable val0, Typeable abs0, Typeable val1, Typeable abs1)
           => String
           -> VectorRepr val0 abs0 -> VectorRepr val1 abs1 -> VectorRepr val abs
           -> (Vector val0 -> Vector val1 -> IO (Vector val))
           -> Flow abs0 -> Flow abs1 -> Kernel abs
vecKernel2 name repr0 repr1 rrepr code = kernel name code' (repr0 :. repr1 :. HNil) rrepr
  where code' [vec,vec1] = castVector <$> code (castVector vec) (castVector vec1)
