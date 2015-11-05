{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
-- |
-- Compilation @[Step] → AST@
module Flow.DnaCompiler where

import Control.Arrow (Arrow(..))
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict ((!))
import qualified Data.HashSet        as HS
import Data.Typeable
import Data.Hashable
import Data.List
import Data.Monoid
import Data.Foldable    (Foldable(foldMap),toList)
import Data.Traversable (Traversable)
import Bound
import Prelude.Extras
import GHC.Generics (Generic)
import Text.PrettyPrint hiding ((<>))

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
    --
    -- * Domain ID
    -- * Action to generate new region.
  | SKern KernelBind [StepE a] [StepE a]
    -- ^ Kernel call
    --
    -- * Kernel description
    -- * Parameters
    -- * Output domain
  | SSplit (StepE a) Int RegSplit (Scope () StepE a)
    -- Split command
    --
    -- * Parent domain
    -- * Domain id of domain being split
    -- * Split function
    -- * Subexpression
  | SDistribute (StepE a) (StepE a)
    -- * Domain
    -- * Steps
  | SSeq  (StepE a) (StepE a)
    -- ^ Sequence two monadic actions
  | SBind (StepE a) (Scope () StepE a)
    -- ^ Monadic bind. We only introduce new variables in monadic context.

  | SActorGrp Int [StepE a]
    -- ^ Corresponds to group of actor
    --
    -- * Actor ID
    -- * Input parameters
  | SActorRecv
    -- ^ Receive data from actors


  | SYieldVec (StepE a)
    -- ^ Yield vector
  deriving (Show,Functor,Foldable,Traversable,Generic)
instance Show1 StepE

{-
-- | Provides foldable instance where we don't go down into
-- split\/distribute combinators
newtype NoNested a = NoNested (StepE a)

instance Foldable NoNested where
  foldMap f (NoNested a) = case a of
    V a      -> f (NoNested a)
    Pair a b -> foldMap f (NoNested a) <> foldMap f (NoNested b)
    List xs  -> mconcat $ map (foldMap f . NoNested) xs
    SDom{}   -> mempty
    SKern _ xs ys -> mconcat $ map (foldMap f . NoNested) xs ++ map (foldMap f . NoNested) xs
    SSplit{}      -> mempty
    SDistribute{} -> mempty
    SSeq a b    -> foldMap f (NoNested a) <> foldMap f (NoNested b)
    SBind a b   -> foldMap f (NoNested a) <> foldMap f (NoNested b)
    SYieldVec a -> foldMap f (NoNested a)
-}

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
  | ChanVar Int
  deriving (Show,Eq,Generic)

instance Hashable V


instance Applicative StepE where
  pure  = V
  (<*>) = ap
instance Monad StepE where
  return = V
  V a              >>= f = f a
  Pair a b         >>= f = Pair (a >>= f) (b >>= f)
  List xs          >>= f = List (map (>>= f) xs)
  SKern k xs ys    >>= f = SKern k (map (>>= f) xs) (map (>>= f) ys)
  SSplit a i r ss  >>= f = SSplit (a >>= f) i r (ss >>>= f)
  SDistribute a ss >>= f = SDistribute (a >>= f) (ss >>= f)
  SSeq  a b        >>= f = SSeq  (a >>= f) (b >>= f)
  SBind e g        >>= f = SBind (e >>= f) (g >>>= f)
  SYieldVec e      >>= f = SYieldVec (e >>= f)


----------------------------------------------------------------
-- Preprocessing of [Steps]
----------------------------------------------------------------


-- | Tree of actors.
data ActorTree a = ActorTree a (HM.HashMap Int (ActorTree a))
                   deriving (Show,Eq,Functor)

walkActorTree :: b -> (a -> a -> b) -> ActorTree a -> ActorTree b
walkActorTree b f (ActorTree a hm)
  = ActorTree b (fmap (go a) hm)
  where
    go a0 (ActorTree a' hmm) = ActorTree (f a0 a') (fmap (go a') hmm)


-- | Variables as used in Step definition
data VV
  = KVar Int [Int]
  | DVar Int
  deriving (Show,Eq,Generic)
instance Hashable VV

-- | Extended Step data type
data ExtStep where
  Step :: Step -> ExtStep
  Call :: Typeable a => Domain a -> [VV] -> Int  -> ExtStep
  -- SplitDistr
  --   :: (Typeable a)
  --   => Domain a                 -- From distribute
  --   -> Schedule
  --   -> [ExtStep]
  --   -> ExtStep
  Expect :: [VV] -> ExtStep
  Yield  :: [VV] -> ExtStep
  Gather :: [VV] -> ExtStep

-- | Actor type
data ActorTy
  = SimpleActor
  | DistrActor  Int Int
  deriving (Show)

-- | Variables for actor definition
data Vars = Vars
  { varsUsed    :: HS.HashSet VV
  , varsProd    :: HS.HashSet VV
  , varsMissing :: HS.HashSet VV
  }
  deriving (Show)


{-

  [Step]



  ActorTree $ [ExtStep] × How this actor invoлed

Actor
  = BaseActor
  | SplitDistrActor



-}




-- Compilation algo:
--
--  1. Separate actors
--  2. Find out input and output parameters for actors
--  3. Insert gather for each use of output
--  4. Convert to AST

-- Split off code for DNA actors
makeActorTree :: [Step] -> ActorTree (ActorTy, [ExtStep])
makeActorTree steps
  = flip evalState 0
  $ run
  $ fmap ((,) SimpleActor)
  $ go steps
  where
    run :: Monad m => WriterT [(Int, ActorTree a)] m a -> m (ActorTree a)
    run m = do (a,pairs) <- runWriterT m
               return $ ActorTree a (HM.fromList pairs)
    --
    go :: [Step] -> WriterT [(Int, ActorTree (ActorTy, [ExtStep]))] (State Int) [ExtStep]
    go []     = return []
    go (x:xs) = do
      x' <- case x of
        (SplitStep dh [DistributeStep dh' sched ss]) -> do
          n <- get
          put $! n+1
          let Just pdom = dhParent dh
          child <- lift
                 $ run
                 $ fmap ((,) (DistrActor (dhId pdom) (dhId dh')))
                 $ go ss
          tell [(n , child)]
          return (Call dh [] n)
        _ -> return (Step x)
      xs' <- go xs
      return $ x' : xs'

-- Determine inputs and outputs for actors
findInOut :: Functor f => ActorTree (f [ExtStep]) -> ActorTree (f (Vars,[ExtStep]))
findInOut = (fmap . fmap) makeVars
  where
    makeVars ast = (Vars u p (HS.difference u p),ast)
      where
        u = foldMap collectReferencedVars ast
        p = foldMap collectProducedVars   ast

-- Add explicit data transfer commands
addCommands :: ActorTree (ActorTy, (Vars,[ExtStep])) -> ActorTree (ActorTy, (Vars,[ExtStep]))
addCommands
  = fmap (\(_,_,a) -> (a))
  . transform (Vars mempty mempty mempty)
  where
    -- Parameters: variables defined in parent and missing in child
    transform pvars (ActorTree (ty,(vars,steps)) children)
      = ActorTree (params,retV,(ty,(vars,steps'''))) children''
      where
        -- Parameters for current actor:
        --   * Variables generated by parent by not in child.
        params = varsProd pvars `HS.intersection` varsMissing vars
        -- Return values
        --   * Variables missing in parent and produced in child when
        --     adjusted for domain
        retV = case ty of
          SimpleActor           -> mempty
          DistrActor dhPar dhCh ->
            (varsProd vars)
            `HS.intersection`
            HS.map (changeDom dhPar dhCh) (varsMissing pvars)

        -- Transform child actors recursively
        children'  = transform vars <$> children
        children'' = children'
        -- Reference parameter from children
        steps' = flip concatMap steps $ \case
          Call dh _ i -> let ActorTree (p,rv,_) _ = children' ! i
                         in [ Call dh (HS.toList p) i
                              -- FIXME: Here we ignore wrong domain
                            , Expect $ HS.toList rv
                            ]
          x -> [x]
        -- Prepend expect parameters if it isn't done
        steps'' | HS.null params = steps'
                | otherwise      = Expect (HS.toList params) : steps'
        -- Append yield
        steps''' | HS.null retV = steps''
                 | otherwise    = steps'' ++ [Yield $ HS.toList retV]



changeDom :: Int -> Int -> VV -> VV
changeDom old new (KVar k ds) = KVar k ((\i -> if i == old then new else i) <$> ds)
changeDom _ _ v = v



-- Collect variables referenced by step
collectReferencedVars :: ExtStep -> HS.HashSet VV
collectReferencedVars = withExtStep collectReferencedVars'

collectProducedVars :: ExtStep -> HS.HashSet VV
collectProducedVars = withExtStep collectProducedVars'

withExtStep f = \case
  Step s -> f s
  Call dh _ _ -> f (SplitStep dh [])
  -- SplitDistr dh' sched steps
  --   -> (f (DistributeStep dh' sched []))
  --   <> foldMap (withExtStep f) steps

-- Collect variables referenced by step
collectReferencedVars' :: Step -> HS.HashSet VV
collectReferencedVars' = \case
  DomainStep dh    -> mempty
  KernelStep kb    -> HS.fromList $ concat
    [ KVar kid (reprDomain repr)
    : (DVar <$> reprDomain repr)
    | KernelDep kid (ReprI repr) <- kernDeps kb
    ]
  -- singleton $ KernVar $ kernId kb
  SplitStep  dh ss ->
    let Just parD = dhParent dh
    in    (HS.singleton $ DVar $ dhId parD)
       <> foldMap collectReferencedVars' ss
  DistributeStep dh _ ss -> (HS.singleton $ DVar $ dhId dh)
                   <> foldMap collectReferencedVars' ss

-- Collect variables produced by step
collectProducedVars' :: Step -> HS.HashSet VV
collectProducedVars' = \case
  DomainStep dh -> HS.singleton $ DVar $ dhId dh
  KernelStep kb -> case kernRepr kb of
    ReprI repr -> HS.singleton $ KVar (kernId kb) (reprDomain repr)
  SplitStep  dh ss       -> (HS.singleton $ DVar $ dhId dh)
                         <> foldMap collectProducedVars' ss
  DistributeStep dh _ ss -> foldMap collectProducedVars' ss





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
       (V $ DomVar (maybe (error "Domain doesn't have parent") dhId $ dhParent dh))
       (dhId dh)
       (RegSplit $ dhRegion dh)
       (abstract1 (DomVar (dhId dh)) $ compileSteps steps)
  where
    kbDomList kb = case kernRepr kb of
      ReprI r -> V . DomVar <$> reprDomain r


----------------------------------------------------------------
-- AST transformations
----------------------------------------------------------------

-- Algorithm outline:
--
-- 1. Separate parallel parts into additional programs:
--
--    [Step] → ActorTree ([Step + Extra commands])
--
-- 2. Determine input/output parameters for commands


-- evalAST :: StepV V -> Maybe V
-- evalAST = undefined

{-
-- | Rewrite AST using given rule until fixed point is reached
rewriteAST :: (StepE V -> Maybe (StepE V)) -> StepE V -> StepE V
rewriteAST rule ast = case loop ast of
  Pair a b             -> Pair (go a) (go b)
  List xs              -> List (map go xs)
  SKern kb xs ys       -> SKern kb (map go xs) (map go ys)
  SSplit e did fun lam ->
    let var = DomVar did
    in SSplit e did fun (abstract1 var $ go $ instantiate1 var lam)
  SDistribute a b -> SDistribute (go a) (go b)
  SBind expr lam  -> undefined
  SSeq        a b -> SSeq (go a) (go b)
  SActorGrp n xs  -> SActorGrp n (map go xs)
  x -> x
  where
    -- Rewrite until fixed point is reached
    loop a = case rule a of
      Just a' -> loop a'
      Nothing -> a
    -- Recurse down and generate (optionally) value for the AST fragment
    go    = rewriteAST rule
    recur ast' = case ast' of
      V v      -> (Just v, ast')
      Pair a b -> (Nothing, Pair (go a) (go b))
      List xs  -> (Nothing, List (map go xs))
      SKern kb xs ys -> ( Just (kernId kb)
                        ,
-}

addYields :: Eq a => StepE a -> StepE a
addYields ast
  = undefined
  where
    -- 1.

    -- List of free variables
    freeVars = nub $ toList ast
    -- freeVecs = [ i | KernVar i <- freeVars ]
    -- Traverse AST and add yield operations for values yielded
    trvDistr x = case x of
      -- Go deeper
      SSeq  a b   -> SSeq  (trvDistr a) (trvDistr b)
      SBind e lam -> SBind (trvDistr e) undefined
      -- We only recognize split immediately followed by distribute
      SSplit dh did f _ -> -- (SDistribute dd steps) ->
        undefined
      -- everything else passed unchanged
      _ -> x






-- Now we need to find out values which needed to be passed as parameters
-- and received as results
--
-- 1. Parameter: free var in child and
-- 2. Result:    free var in parent, defined in child
--
-- To that end we build sets of free and defined vars both in child
-- and parent


-- We also need to merge vectors for each vector result. To that end
-- we need to pass regions with buffers













{-

-- | Generate actor definition for each actor.
--
--  1. We nonrecursively replace each split→distribute pair with actor
--    call and generate map of such calls. Input/output parameters are
--    not handled at this stage
separateActors :: StepE V -> (StepE V, HM.Int (StepE V))
separateActors = undefined

-- | Get parameters which are needed for an actor
obtainParameters :: StepE V -> ???
obtainParameters = undefined
-}





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



----------------------------------------------------------------
-- Pretty printer
----------------------------------------------------------------

-- | Pretty print AST
prettyprint :: Show a => StepE a -> Doc
prettyprint = flip evalState varNames . ppr . fmap Right
  where
    varNames = map (:[]) ['a' .. 'z']
    pprList es = do ss <- mapM ppr es
                    return $ brackets $ hcat $ intersperse comma ss
    ppr = \case
      V (Left  v) -> return $ text v
      V (Right a) -> return $ text (show a)
      Pair e g    -> do se <- ppr e
                        sg <- ppr g
                        return $ parens $ se <> comma <> sg
      List es     -> pprList es
      SDom i r    -> return $ text (show r) <> text " " <> int i
      SKern kb vars dom -> do
        vs <- pprList vars
        ds <- pprList dom
        return $ text "Kernel call" $$ nest 2
          (vcat [ text (show kb), vs, ds ])
      SSeq e1 e2 -> liftM2 ($$) (ppr e1) (ppr e2)
      SSplit e i _ steps -> do
        v  <- fresh
        se <- ppr e
        ss <- ppr $ instantiate1 (V $ Left v) steps
        (v : rest) <- get
        put rest
        return $ (text "Split: " <> se <> text " to " <> int i <> text "  λ " <> text v <> text " →")
              $$ nest 2 ss
      SDistribute e steps -> do
        se <- ppr e
        ss <- ppr steps
        return $ (text "Distribute " <> se) $$ nest 2 ss
      SBind e lam -> do
        v  <- fresh
        se <- ppr e
        sl <- ppr $ instantiate1 (V $ Left v) lam
        return $ vcat
          [ se <> text (" $ λ"++v++" →")
          , nest 2 sl
          ]
      --
      SYieldVec v -> do
        s <- ppr v
        return $ text "YieldVec " <> s
    fresh = do
      v : rest <- get
      put rest
      return v
