{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
-- |
-- AST for compilation to DNA
module AST (
    -- * AST definition
    Exp(..)
  , ActorRepr(..)
  , DVar(..)
    -- * Conversion to DNA
  , Box(..)
  , compileToDNA
  ) where

import Bound
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import GHC.Generics (Generic,Generic1)

import Control.Distributed.Process
import Control.Distributed.Process.Closure (mkClosureValSingle,MkTDict)
import DNA



----------------------------------------------------------------
-- AST definition
----------------------------------------------------------------

-- | AST for DNA program. At the moment we're assuming all values have
-- same type @Box@. Below are typing rules
data Exp a
  = V a
    -- ^ Variable. Its type
  | Kern ([Box] -> IO Box) [Exp a]
    -- ^ Call of kernel
  | Arc  [ActorRepr] (Exp a)
    -- ^ Arc of actors
  | Bind (Exp a) (Scope () Exp a)
    -- ^ Monadic bind
  deriving (Functor,Foldable,Traversable, Generic,Generic1)

-- | Representation of an actor
data ActorRepr = ActorReprs (Scope () Exp DVar)

data DVar = DVar Int

instance Applicative Exp where
  pure = return
  (<*>) = ap
instance Monad Exp where
  return = V
  V a        >>= f = f a
  Kern s a   >>= f = Kern s (map (f =<<) a)
  Arc arc x  >>= f = Arc arc (f =<< x)
  Bind m lam >>= f = Bind (m >>= f) (lam >>>= f)



----------------------------------------------------------------
-- Interpretation
----------------------------------------------------------------

-- | Wrapper type for all supported data types in DNA.
--
--   At the moment simply wrapped Int
newtype Box = Box Int
              deriving (Show,Eq, Binary, Typeable)

-- | Convert expression to DNA program
compileToDNA :: Exp DVar -> Either String (DNA Box, RemoteTable -> RemoteTable)
compileToDNA e = do
  (dna,Endo rt) <- runExcept
                 $ runWriterT
                 $ flip evalStateT 0 
                 $ convertToDNA e
  return (dna,rt)

-- Monad for compiling  
type Compiler = StateT Int (WriterT (Endo RemoteTable) (Except String))

-- Type class for DNA-like monad. Useful for interpreter
class Monad m => MonadDNA m where
  liftDNA :: DNA a -> m a

instance MonadDNA DNA where
  liftDNA = id
instance MonadDNA m =>MonadDNA (ReaderT t m) where
  liftDNA = lift . liftDNA 




-- | Convert closed expression to DNA expression.
convertToDNA :: Exp DVar -> Compiler (DNA Box)
convertToDNA e = case closed e of
  Just a  -> convert a
  Nothing -> throwError "Expression contains free variables"

-- Convert 
convert :: MonadDNA m => Exp (m Box) -> Compiler (m Box)
convert = \case 
  V{}           -> throwError "pure variable at the top level"
  -- Generate kernel call
  Kern fun vars -> do
    xs <- mapM toPure vars
    return $ do
      ys <- sequence xs
      liftDNA $ kernel "kernel" [] $ liftIO (fun ys)
  -- Compile spawning of actors
  Arc arc var -> do
    param <- toPure var
    acts  <- mapM compileActor arc
    -- FIXME: here I wait on promise immediately
    return $ do
      x      <- param
      shells <- sequence acts
      liftDNA $ sendParam x (head shells)
      liftDNA $ forM_ (shells `zip` tail shells) $ \(shA,shB) ->
        connect shA shB
      fut <- liftDNA $ delay Remote (last shells)
      liftDNA $ await fut
  -- Monadic bind
  Bind expr lam -> do
    dna <- convert expr
    convert $ instantiate1 (V dna) lam

-- Generate code for starting actor
compileActor :: MonadDNA m => ActorRepr -> Compiler (m (Shell (Val Box) (Val Box)))
compileActor (ActorReprs lam) = do
  case closed lam of
    Nothing -> throwError "Free variables!"
    Just f -> do
      -- Generate code for actor in reader monad
      act  <- convert $ instantiate1 (V ask) f
      clos <- closurise $ actor $ runReaderT act
      -- Unwrap reader and generate closure
      return $ liftDNA $
        startActor (N 0) $ do
          useLocal
          return clos

-- Convert expression to pure value. At the moment language is not very rich.
toPure :: Exp a -> Compiler a
toPure (V x) = return x
toPure _     = throwError "toPure: nonvariable"

-- Make closure from value
closurise :: (Typeable a, MkTDict a) => a -> Compiler (Closure a)
closurise a = do
  oldN <- get
  put $! oldN + 1
  let fresh = "DNA_NAME_" ++ show oldN
  let (clos,reg) = mkClosureValSingle fresh $ \_ -> a
  tell $ Endo $ reg
  return $ clos ()
