module DNA.Compiler.Types (
    -- * Data types
    Compile
  , CompileA
  , compile
  , applicatively
  , freshName
  , fresh
  , compError
    -- * Applicative either
  , EitherA(..)
  , leftA
  , toExceptT
  , toEither
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Functor.Identity

import Data.Monoid

----------------------------------------------------------------
-- Common data types
----------------------------------------------------------------

-- | Type synonym for compilation monad
type Compile = ExceptT [String] (State Int)

type CompileA = EitherA [String] (State Int)

compile :: Compile a -> Either [String] a
compile
  = flip evalState 0
  . runExceptT 

applicatively :: CompileA a -> Compile a
applicatively = toExceptT

freshName :: Compile String
freshName = fresh "x"

fresh :: String -> Compile String
fresh pref = do
  i <- lift get
  lift $ put $! i + 1
  return $ pref ++ "_" ++ show i


compError :: [String] -> Compile a
compError = throwE


----------------------------------------------------------------
-- Variant of Expect monad which collects all errors
----------------------------------------------------------------

-- | Applicative error. It collects all exceptional conditions
newtype EitherA e f a = EitherA { runEitherA :: f (Either e a) }

instance Functor f => Functor (EitherA e f) where
  fmap f = EitherA . fmap (fmap f) . runEitherA
  {-# INLINE fmap #-}

instance (Monoid e, Applicative f) => Applicative (EitherA e f) where
  pure a  = EitherA $ pure (Right a)
  {-# INLINE pure #-}
  EitherA f <*> EitherA a = EitherA $ combine <$> f <*> a
    where
      combine (Left  e1) (Left  e2) = Left  (e1 <> e2)
      combine (Left  e ) (Right _ ) = Left  e
      combine (Right _ ) (Left  e ) = Left  e
      combine (Right g ) (Right x ) = Right (g x)
  {-# INLINE (<*>) #-}


leftA :: Applicative f => e -> EitherA e f a
leftA = EitherA . pure . Left

toExceptT :: Monad m => EitherA e m a -> ExceptT e m a
toExceptT = ExceptT . runEitherA

toEither :: EitherA e Identity a -> Either e a
toEither = runIdentity . runEitherA
