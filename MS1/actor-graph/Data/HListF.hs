{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
-- | Heterogeneous lists
module Data.HListF (
    HListF(..)
    -- * Generic functions
  , sequenceHListF
  , monomorphize
  , mapF
  , forHListF
  , iforHListF
  ) where

import Control.Applicative
import Data.Binary
import Data.Typeable
import Control.Distributed.Process.Serializable
import Data.Functor.Compose



-- | Heterogeneous list of data types sharing same type constructor
--
--   Here we carry Serializable dictionary for every element
data HListF xs f where
  ConsF :: (Serializable x) => f x -> HListF xs f -> HListF (x ': xs) f
  NilF  :: HListF '[] f

instance Binary (HListF '[] f) where
  get      = return NilF
  put NilF = return ()

instance (Binary (f x), Binary x, Typeable x,  Binary (HListF xs f)) => Binary (HListF (x ': xs) f) where
  get = ConsF <$> get <*> get
  put (ConsF x xs) = put x >> put xs

data Proxy a = Proxy

class TypeableList (xs :: [*]) where
  typeOfList :: p xs -> TypeRep

instance TypeableList '[] where
  typeOfList _
    = mkTyConApp (mkTyCon3 "GHC" "GHC.TypeLits" "'[]") []
instance (Typeable x, TypeableList xs) => TypeableList (x ': xs) where
  typeOfList _
    = mkTyConApp (mkTyCon3 "GHC" "GHC.TypeLits" "':")
      [ typeOf (undefined :: x)
      , typeOfList (Proxy :: Proxy xs)
      ]
instance (Typeable1 f, TypeableList xs) => Typeable (HListF xs f) where
  typeOf _
    = mkTyConApp (mkTyCon3 "actor-graph" "Data.HListF" "HListF")
      [ typeOfList (Proxy :: Proxy xs)
      , typeOf1    (undefined :: f ())
      ]


-- | Float outermost monad out. It's similar to 'Control.Monad.sequence'
sequenceHListF :: Monad m => HListF xs (m `Compose` f) -> m (HListF xs f)
sequenceHListF NilF = return NilF
sequenceHListF (ConsF (Compose m) rest) = do
  x  <- m
  xs <- sequenceHListF rest
  return $ ConsF x xs

monomorphize :: (forall a. Serializable a => f a -> x) -> HListF xs f -> [x]
monomorphize _ NilF = []
monomorphize f (ConsF x xs) = f x : monomorphize f  xs

-- | Change type constructor of list
mapF :: (forall a. f a -> g a) -> HListF xs f -> HListF xs g
mapF _ NilF = NilF
mapF f (ConsF x xs) = f x `ConsF` mapF f xs


-- | Execute monadic action for every element in list
forHListF :: Monad m
          => HListF xs f
          -> (forall a. Serializable a => f a -> m ())
          -> m ()
forHListF  NilF        _ = return ()
forHListF (ConsF x xs) f = f x >> forHListF xs f


-- | Execute monadic action which takes element index as well for
--   every element in list
iforHListF :: Monad m
           => HListF xs f
           -> (forall a. Serializable a => Int -> f a -> m ())
           -> m ()
iforHListF = go 0
  where
    go :: Monad m => Int -> HListF xs f -> (forall a. Serializable a => Int -> f a -> m ()) -> m ()
    go _  NilF        _ = return ()
    go i (ConsF x xs) f = f i x >> go (i+1) xs f
