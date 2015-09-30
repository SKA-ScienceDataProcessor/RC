{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Strategy.Data
  ( DataRepr(..), ReprAccess(..)
  , NoRepr(..), CBufRepr(..)
  ) where

import Strategy.Internal

import Data.Typeable

-- ----------------------------------------------------------------------------
-- ---                        Data Representations                          ---
-- ----------------------------------------------------------------------------

-- No representation: Either don't produce anything (= nobody can use
-- result) or don't care about input (= accept any input).
data NoRepr a = NoRepr
  deriving Typeable
instance Typeable a => Show (NoRepr a) where
  show _ = "nothing [" ++ show (typeOf (undefined :: a)) ++ "]"
instance Typeable a => DataRepr (NoRepr a) where
  type RPar (NoRepr a) = a
  reprNop _ = True
  reprAccess _ = ReadAccess
  reprCompatible _ _ = True

-- C buffer representation
data CBufRepr a = CBufRepr ReprAccess
  deriving Typeable
instance Typeable a => Show (CBufRepr a) where
  show _ = "C buffer [" ++ show (typeOf (undefined :: a)) ++ "]"
instance Typeable a => DataRepr (CBufRepr a) where
  type RPar (CBufRepr a) = a
  reprAccess (CBufRepr acc) = acc

