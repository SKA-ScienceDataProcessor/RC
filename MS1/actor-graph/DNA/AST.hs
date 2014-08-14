{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module DNA.AST where

import qualified Data.Vector.Storable as S
import Data.Typeable


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

  -- Fold
  Fold :: (Expr env (a -> a -> a))
       -> Expr env a
       -> Expr env (Array sh a)
       -> Expr env a
  -- Zip two vectors
  Zip  :: (Expr env (a -> b -> c))
       -> Expr env (Array sh a)
       -> Expr env (Array sh b)
       -> Expr env (Array sh c)
  -- Generate vector
  Generate :: Expr env sh
           -> Expr env (Int -> a)
           -> Expr env (Array sh a)

  -- Primitive operations
  Add :: Num a => Expr env (a -> a -> a)
  Mul :: Num a => Expr env (a -> a -> a)

  Out :: [Outbound env]
      -> Expr env Out

  -- Scalars
  Scalar :: IsScalar a => a -> Expr env a
  Tup2   :: Expr env a -> Expr env b -> Expr env (a,b)
  String :: String -> Expr env String
  -- Array sizes
  EShape :: Shape -> Expr env Shape
  ESlice :: Slice -> Expr env Slice
  -- Primitive array
  Vec :: Array sh a -> Expr env (Array sh a)

  -- FFI


data Idx env t where
  ZeroIdx ::              Idx (env,t) t
  SuccIdx :: Idx env t -> Idx (env,s) t


newtype Shape = Shape Int

data Slice = Slice Int Int

data Array sh a = Array sh (S.Vector a)



----------------------------------------------------------------
-- Connection encoding
----------------------------------------------------------------

-- | Type tag for expressions for sending data
data Out

-- | ID of outgoing connection
newtype ConnId = ConnId Int
               deriving (Show)

-- | Connection nor bound into dataflow graph
data ConnSimple a where
  ConnSimple :: Typeable a => ConnId -> ConnSimple a

-- | Connection for scattering data to several worker processes
data ConnScatter a where
  ConnScatter :: Typeable a => ConnId -> Int -> ConnScatter a

-- | Outgoing message
data Outbound env where
  -- | Simple outgoing connection
  Outbound :: ConnSimple a
           -> Expr env a
           -> Outbound env
  -- | Outgoing connection for scattering data
  Scatter  :: ConnScatter a
           -> Expr env [a]
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


----------------------------------------------------------------
-- JUNK
----------------------------------------------------------------

-- data NumDict a where
--   NumDict :: Num a => NumDict a
-- instance IsNum a where
--   reify
