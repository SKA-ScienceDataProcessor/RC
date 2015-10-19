{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
-- |
-- Compilation @[Step] → AST@
module Step where

import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Foldable    (Foldable)
import Data.Traversable (Traversable)
import Bound

import Flow
import Flow.Internal
import Flow.Vector
import DNA


-- |
-- Untyped expression tree for compiling to DNA
data StepE a
  = V a
    -- ^ Untyped variable
  | Pair (StepE a) (StepE a)
    -- ^ 2-tuple 
  | List [StepE a]
    -- ^ List of variables

  | SDom  (IO Domain)
    -- ^ Create domain
  | SKern KernelBind [StepE a] [StepE a]
    -- ^ Kernel call
    -- 
    -- * Kernel description
    -- * Parameters
    -- * Output domain 
  | SBind (StepE a) (Scope () StepE a)
    -- ^ Monadic bind. We only introduce new variables in monadic context.
  deriving (Functor,Foldable,Traversable)

-- | Variable name
data V
  = KernVar Int
  | DomVar  Int
  deriving (Show,Eq)

instance Applicative StepE where
  pure  = return
  (<*>) = ap
instance Monad StepE where
  return = V
  V a           >>= f = f a
  Pair a b      >>= f = Pair (a >>= f) (b >>= f)
  List xs       >>= f = List (map (>>= f) xs)
  SKern k xs ys >>= f = SKern k (map (>>= f) xs) (map (>>= f) ys)
  SBind e g     >>= f = SBind (e >>= f) (g >>>= f)


----------------------------------------------------------------
-- Compilation to expression tree
----------------------------------------------------------------

compileSteps :: [Step] -> StepE V
compileSteps []     = error "compileSteps: empty list"
compileSteps [x]    = fst $ singleStep x
compileSteps (x:xs) =
  let (expr,v) = singleStep x
      rest     = compileSteps xs
  in expr `SBind` abstract1 v rest

singleStep :: Step -> (StepE V, V)
singleStep = \case
  DomainStep dh    -> (SDom (dhCreate dh), DomVar  (dhId dh))
  KernelStep kb    -> ( SKern kb
                        [ Pair (V $ KernVar i) (List $ V . DomVar <$> ds)
                        | (i,ds) <- kernDeps kb ]
                        (kbDomList kb)
                      , KernVar (kernId kb))
  DistributeStep{} -> error "DistributeStep is not supported"
  SplitStep{}      -> error "SplitStep is not supported"
  where
    kbDomList kb = case kernRepr kb of
      ReprI r -> V . DomVar <$> reprDomain r


----------------------------------------------------------------
-- Interpretation
----------------------------------------------------------------

data Box
  = VDom Domain
  | VVec (Vector ())

interpretAST :: StepE V -> DNA Box
interpretAST e = case closed e of
  Just e' -> go e'
  Nothing -> error "interpretAST: expression is not closed!"
  where
    go = \case
      V{}    -> error "Naked variable at the top level"
      Pair{} -> error "Naked pair at the top level"
      List{} -> error "Naked list at the top level"
      -- Create new domain
      SDom  dom         ->
        DNA.kernel "dom" [] $ liftIO $ VDom <$> dom
      -- Call kernel
      SKern kb deps dom ->
        let toDom = \case
              V (VDom d) -> d
              V (VVec _) -> error "Vector where domain expected"
              _          -> error "Only variables expected"
            toParam = \case
              Pair (V (VVec v)) (List p) -> (v, map toDom p)
              _                          -> error "Ill formed parameters"
            xs  = map toParam deps
            out = map toDom   dom
        in DNA.kernel "kern" [] $ liftIO $ VVec <$> kernCode kb xs out
      -- Monadic bind
      SBind expr lam ->
        let dnaE = go expr
            lamE = \a -> go $ instantiate1 (V a) lam
        in dnaE >>= lamE
