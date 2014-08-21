{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module DNA.AST (
    -- * AST
    Expr(..)
  , Idx(..)
    -- ** Tuple
  , Tuple(..)
  , TupleIdx(..)
  , IsTuple(..)
  , Arity(..)
    -- ** Array and array shaped
  , Array(..)
  , Shape(..)
  , Slice(..)
    -- ** Connection encoding
  , Out
  , ConnId(..)
  , ConnType(..)
  , Conn(..)
  , Outbound(..)
    -- ** Dictionaries
  , ScalarDict(..)
  , IsScalar(..)
  , ShapeDict(..)
  , IsShape(..)
  , VectorDict(..)
  , IsVector(..)
  , ValueDict(..)
  , IsValue(..)
  ) where

import Control.Applicative
import qualified Data.Vector.Storable as S
import Data.Typeable
import Data.Functor.Identity
import Data.Binary (Binary)
import GHC.Generics (Generic)



----------------------------------------------------------------
-- AST
----------------------------------------------------------------

-- | AST for expression.
data Expr env a where
  -- | Local let binding
  Let :: Expr env expr          -- Bound expression
      -> Expr (env,expr) a      -- Expression in scope of let
      -> Expr env a
  -- | Variable bound by let
  Var :: Idx env a
      -> Expr env a
  -- | Apply
  Ap  :: Expr env (a -> b)
      -> Expr env a
      -> Expr env b
  -- | Lambda abstraction
  Lam :: IsValue a
      => Expr (env,a) b
      -> Expr env (a -> b)

  -- | Fold
  Fold :: (Expr env (a -> a -> a))
       -> Expr env a
       -> Expr env (Array sh a)
       -> Expr env a
  -- | Zip two vectors
  Zip  :: (Expr env (a -> b -> c))
       -> Expr env (Array sh a)
       -> Expr env (Array sh b)
       -> Expr env (Array sh c)
  -- | Generate vector
  Generate :: Expr env sh
           -> Expr env (Int -> a)
           -> Expr env (Array sh a)

  -- Primitive operations
  Add :: Num a => Expr env (a -> a -> a)
  Mul :: Num a => Expr env (a -> a -> a)
  FromInt :: Expr env (Int -> Double)

  Out :: [Outbound env]
      -> Expr env Out

  -- Scalars
  Scalar :: IsScalar a => a -> Expr env a
  -- | Tuple expression
  Tup    :: IsTuple tup => Tuple (Expr env) (Elems tup) -> Expr env tup
  -- | Tuple projection
  Prj    :: TupleIdx (Elems tup) a -> Expr env (tup -> a)
  String :: String -> Expr env String
  -- Array sizes
  EShape :: Shape -> Expr env Shape
  ESlice :: Slice -> Expr env Slice
  -- Primitive array
  Vec :: Array sh a -> Expr env (Array sh a)

  -- | List literal
  List :: IsScalar a => [a] -> Expr env [a]
  -- | Functor instance for list
  FMap :: Expr env (a -> b) -> Expr env [a] -> Expr env [b]
  -- | Special form for scattering values
  ScatterShape :: Expr env (Int -> Shape -> [Slice])
  -- FFI

-- | De-Bruijn index for variable
data Idx env t where
  ZeroIdx ::              Idx (env,t) t
  SuccIdx :: Idx env t -> Idx (env,s) t


----------------------------------------------------------------
-- Tuples
----------------------------------------------------------------

-- | Encoding for a tuple
data Tuple :: (* -> *) -> [*] -> * where
  Nil  :: Tuple f '[]
  Cons :: f a -> Tuple f as -> Tuple f (a ': as)
infixr `Cons`

-- | Index for tuple index
data TupleIdx xs e where
  Here  :: Arity xs =>      TupleIdx (x ': xs) x
  There :: TupleIdx xs e -> TupleIdx (x ': xs) e

class Arity (xs :: [*]) where
  arity :: p xs -> Int

instance Arity '[] where
  arity _ = 0
instance Arity xs => Arity (x ': xs) where
  arity p = 1 + arity (sub p)
    where
      sub :: p (x ': xs) -> p xs
      sub _ = undefined


class IsTuple a where
  type Elems a :: [*]
  toRepr   :: a -> Tuple Identity (Elems a)
  fromRepr :: Tuple Identity (Elems a) -> a

instance IsTuple (a,b) where
  type Elems (a,b) = '[a,b]
  toRepr (a,b) = pure a `Cons` pure b `Cons` Nil
  fromRepr (Identity a `Cons` Identity b `Cons` Nil) = (a,b)
  fromRepr _ = error "Impossible"

instance IsTuple (a,b,c) where
  type Elems (a,b,c) = '[a,b,c]
  toRepr (a,b,c) = pure a `Cons` pure b `Cons` pure c `Cons` Nil
  fromRepr (Identity a `Cons` Identity b `Cons` Identity c `Cons` Nil) = (a,b,c)
  fromRepr _ = error "Impossible"

newtype Shape = Shape Int
                deriving (Show,Eq,Typeable,Generic)
instance Binary Shape

data Slice = Slice Int Int
           deriving (Show,Eq,Typeable,Generic)
instance Binary Slice

data Array sh a = Array sh (S.Vector a)



----------------------------------------------------------------
-- Connection encoding
----------------------------------------------------------------

-- | Type tag for expressions for sending data
data Out

-- | ID of outgoing connection. Each actor can have several outgoing
--   connections which are identified by tat ID.
newtype ConnId = ConnId Int
               deriving (Show,Eq,Ord)

-- | Connection type
data ConnType
  = ConnOne
    -- ^ We allow to connect to one 
  | ConnMany
    -- ^ It could connect to one or more actors
  deriving (Show,Eq,Ord)

-- | Typed wrapper for connection ID.
data Conn a where
  -- | Connection for which only 
  Conn :: Typeable a => ConnId -> ConnType -> Conn a


-- | Outgoing message
data Outbound env where
  -- | Simple outgoing connection
  Outbound :: Conn a
           -> Expr env a
           -> Outbound env
  -- | Sending result of computation
  OutRes   :: Expr env a
           -> Outbound env
  -- | Log message
  PrintInt :: Expr env Int
           -> Outbound env



----------------------------------------------------------------

data ScalarDict a where
  DoubleDict :: ScalarDict Double
  IntDict    :: ScalarDict Int
  UnitDict   :: ScalarDict ()

class IsScalar a where
  reifyScalar :: a -> ScalarDict a

instance IsScalar Double where reifyScalar _ = DoubleDict
instance IsScalar Int    where reifyScalar _ = IntDict
instance IsScalar ()     where reifyScalar _ = UnitDict




data ShapeDict a where
  ShShape :: ShapeDict Shape
  ShSlice :: ShapeDict Slice

class IsShape a where
  reifyShape :: a -> ShapeDict a

instance IsShape Shape where reifyShape _ = ShShape
instance IsShape Slice where reifyShape _ = ShSlice


data VectorDict a where
  VecD :: IsShape sh => VectorDict (Array sh Double)

class IsVector a where
  reifyVector :: a -> VectorDict a

instance IsShape sh => IsVector (Array sh Double) where
  reifyVector _ = VecD



data ValueDict a where
  ValScalar :: IsScalar a => ValueDict a
  ValShape  :: IsShape  a => ValueDict a
  ValVec    :: IsVector a => ValueDict a

class IsValue a where
  reifyValue :: a -> ValueDict a

instance IsValue Int    where reifyValue _ = ValScalar
instance IsValue Double where reifyValue _ = ValScalar
instance IsValue ()     where reifyValue _ = ValScalar

instance IsValue Shape where reifyValue _ = ValShape
instance IsValue Slice where reifyValue _ = ValShape

instance IsShape sh => IsValue (Array sh Double) where
  reifyValue _ = ValVec
