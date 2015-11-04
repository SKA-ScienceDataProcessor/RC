{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
-- |
-- Compilation @[Step] → AST@
module Flow.DnaCompiler where

import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Foldable    (Foldable)
import Data.Traversable (Traversable)
import Bound
import Prelude.Extras
import GHC.Generics (Generic)

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
    -- ^ 2-tuple. At the moment pair of kernel-region box]

  | List [StepE a]
    -- ^ List of variables

  | SDom Int NewRegion
    -- ^ Create domain
  | SKern KernelBind [StepE a] [StepE a]
    -- ^ Kernel call
    -- 
    -- * Kernel description
    -- * Parameters
    -- * Output domain 
  | SSplit (StepE a) RegSplit (StepE a)
    -- Split command
    --
    -- * Domain
    -- * Split function
    -- * Subexpression
  | SDistribute (StepE a) (StepE a)
    -- * Domain
    -- * Steps
  | SSeq  (StepE a) (StepE a)
    -- ^ Sequence two monadic actions
  | SBind (StepE a) (Scope () StepE a)
    -- ^ Monadic bind. We only introduce new variables in monadic context.
  deriving (Show,Functor,Foldable,Traversable,Generic)
instance Show1 StepE

-- | Newtype wrapper for function for splitting regions. Only used to
--   get free Show instance for StepE
newtype RegSplit  = RegSplit (Region -> IO [Region])
newtype NewRegion = NewRegion (IO Region)

instance Show RegSplit where
  show _ = "RegSplit"
instance Show NewRegion where
  show _ = "NewRegion"

-- | Variable name
data V
  = KernVar Int
    -- ^ @Vector ()@ produced by kernel. It's referenced by KernelID
  | DomVar  Int
    -- ^ Region (referenced by DomainID)
  deriving (Show,Eq)

instance Applicative StepE where
  pure  = V
  (<*>) = ap
instance Monad StepE where
  return = V
  V a              >>= f = f a
  Pair a b         >>= f = Pair (a >>= f) (b >>= f)
  List xs          >>= f = List (map (>>= f) xs)
  SKern k xs ys    >>= f = SKern k (map (>>= f) xs) (map (>>= f) ys)
  SSplit a r ss    >>= f = SSplit (a >>= f) r (ss >>= f)
  SDistribute a ss >>= f = SDistribute (a >>= f) (ss >>= f)
  SSeq  a b        >>= f = SSeq  (a >>= f) (b >>= f)
  SBind e g        >>= f = SBind (e >>= f) (g >>>= f)


----------------------------------------------------------------
-- Compilation to expression tree
----------------------------------------------------------------

{-
data AnyDH = forall a. Typeable a => AnyDH (Domain a)
type DataMap = IM.IntMap (ReprI, Map.Map RegionBox (Vector ()))
type DomainMap = IM.IntMap (AnyDH, RegionBox)
-}

compileSteps :: [Step] -> StepE V
compileSteps []     = error "compileSteps: empty list"
compileSteps [x]    = toStep $ singleStep x
compileSteps (x:xs) =
  let rest = compileSteps xs
  in case singleStep x of
       StepVal   expr v -> expr `SBind` abstract1 v rest
       StepNoVal expr   -> expr `SSeq` rest

data StepRes
  = StepVal   (StepE V) V
  | StepNoVal (StepE V)

toStep :: StepRes -> StepE V
toStep = \case
  StepVal   e _ -> e
  StepNoVal e   -> e

singleStep :: Step -> StepRes
singleStep = \case
  DomainStep dh    -> StepVal
                        (SDom (dhId dh) (NewRegion $ dhCreate dh))
                        (DomVar  (dhId dh))
  KernelStep kb    -> StepVal
    (SKern kb
       -- Parameters
       [ Pair (V $ KernVar kid)
              (List $ V . DomVar <$> reprDomain repr)
       | KernelDep kid (ReprI repr) <- kernDeps kb
       ]
       -- Output domains
       (kbDomList kb))
    (KernVar (kernId kb))
  DistributeStep dh _ steps ->
    StepNoVal $
      SDistribute
        (V $ DomVar (dhId dh))
        (compileSteps steps)
  SplitStep dh steps ->
    StepNoVal $ SSplit
       (V $ DomVar (dhId dh))
       (RegSplit $ dhRegion dh)
       (compileSteps steps)
  where
    kbDomList kb = case kernRepr kb of
      ReprI r -> V . DomVar <$> reprDomain r


----------------------------------------------------------------
-- Interpretation
----------------------------------------------------------------

data Box
  = VReg RegionBox
  | VVec (Vector ())

{-
interpretAST :: StepE V -> DNA Box
interpretAST e = case closed e of
  Just e' -> go e'
  Nothing -> error "interpretAST: expression is not closed!"
  where
    go = \case
      V{}    -> error "Naked variable at the top level"
      Pair{} -> error "Naked pair at the top level"
      -- List{} -> error "Naked list at the top level"
      -- Create new domain
      SDom  dom         ->
        DNA.kernel "dom" [] $ liftIO $ VReg <$> dom
      -- Call kernel
      {-
      SKern kb deps dom ->
        let toDom = \case
              V (VReg d) -> d
              V (VVec _) -> error "Vector where domain expected"
              _          -> error "Only variables expected"
            toParam = \case
              Pair (V (VVec v)) (List p) -> (v, map toDom p)
              _                          -> error "Ill formed parameters"
            xs  = map toParam deps
            out = map toDom   dom
        in DNA.kernel "kern" [] $ liftIO $ VVec <$> kernCode kb xs out
-}
      -- Monadic bind
      SBind expr lam ->
        let dnaE = go expr
            lamE = \a -> go $ instantiate1 (V a) lam
        in dnaE >>= lamE
-}
