{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
-- |
-- Compilation @[Step] → AST@
module Flow.DnaCompiler (
    -- * Ext. steps
    ActorTree(..)
  , walkActorTree
    -- ** Extended steps
  , VV(..)
  , ExtStep(..)
  , Vars(..)
    -- ** Transformations
  , makeActorTree
  , findInOut
  , addCommands
    -- * Compilation to AST for DNA
    -- ** AST
  , StepE(..)
  , RegSplit(..)
  , NewRegion(..)
    -- ** Compilation
  , DnaActor(..)
  , V(..)
  , compileProgram
    -- ** Pretty-printing
  , prettyprint
  , prettyprintLam
    -- * Interpretation
  , interpretAST
  ) where

import Control.Arrow (Arrow(..))
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer
import Control.Distributed.Process         (RemoteTable)
import Control.Distributed.Process.Closure (mkClosureValSingle,MkTDict)
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict ((!))
import qualified Data.HashSet        as HS
import qualified Data.Binary as Bin
import Data.Typeable
import Data.Hashable
import Data.List
import Data.Monoid
import Data.Foldable    (Foldable(foldMap),toList)
import Data.Traversable (Traversable(traverse,sequenceA))
import Bound
import Prelude.Extras
import GHC.Generics (Generic)
import Text.PrettyPrint hiding ((<>))

import Flow
import Flow.Internal
import Flow.Vector
import DNA

import Debug.Trace



----------------------------------------------------------------
-- Preprocessing of [Steps]
--
-- We separate steps into actor which are in turn converted to
-- DNA expression
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
  Step   :: Step -> ExtStep
  Call   :: Typeable a => Domain a -> [VV] -> Int  -> ExtStep
  Expect :: Int -> [(Maybe ReprI, VV)] -> ExtStep
  Yield  :: [VV] -> ExtStep

-- | Actor type
data ActorTy
  = SimpleActor
  | DistrActor  Int Int         -- Split/Distribute actor
  | DistrVarActor [VV] [VV]     -- Split/Distribute actor with variables
  deriving (Show)

-- | Variables for actor definition
data Vars = Vars
  { varsUsed    :: HS.HashSet VV -- ^ Variables referenced in actor
  , varsProd    :: HS.HashSet VV -- ^ Variables defined in actor
  , varsMissing :: HS.HashSet VV -- ^ Used variables which are not defined in actor
  }
  deriving (Show)


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
    --
    run :: Monad m => WriterT [(Int, ActorTree a)] m a -> m (ActorTree a)
    run m = do (a,pairs) <- runWriterT m
               return $ ActorTree a (HM.fromList pairs)
    -- Transform each step
    go :: [Step] -> WriterT [(Int, ActorTree (ActorTy, [ExtStep]))] (State Int) [ExtStep]
    go []     = return []
    go (x:xs) = do
      x' <- case x of
        --
        {-
        (SplitStep dh [DistributeStep dh' _sched ss]) -> do
          n <- get
          put $! n+1
          let Just pdom = dhParent dh
          child <- lift
                 $ run
                 $ fmap ((,) (DistrActor (dhId pdom) (dhId dh')))
                 $ go ss
          tell [(n , child)]
          return (Call dh [] n)
        -}
        --
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
  = fmap (\(_,_,a) -> a)
  . transform (Vars mempty mempty mempty)
  where
    -- Parameters: variables defined in parent and missing in child
    transform pvars (ActorTree (ty,(vars,steps)) children)
      = ActorTree (params,retV, (ty',(vars,steps''))) children'
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
            varsProd vars
            `HS.intersection`
            HS.map (changeDom dhPar dhCh) (varsMissing pvars)
        ty' = case ty of
          SimpleActor  -> SimpleActor
          DistrActor{} -> DistrVarActor
              (HS.toList params)
              (HS.toList retV)
        -- Transform child actors recursively
        children'  = transform vars <$> children
        -- Reference parameter from children
        steps' = flip concatMap steps $ \case
          Call dh _ i -> let ActorTree (p,rv,_) _ = children' ! i
                         in [ Call dh (HS.toList p) i
                            , Expect i [ case v of
                                           DVar{} -> (Nothing, v)
                                           KVar kid _ -> ( getFirst $ mconcat $ map (findReprForK kid) steps
                                                         , v
                                                         )
                                       | v <- HS.toList rv
                                       ]
                            ]
          x -> [x]
        appendY = case ty' of
          DistrVarActor _ rv -> (++ [Yield rv])
          _                  -> id
        steps'' = appendY steps'


changeDom :: Int -> Int -> VV -> VV
changeDom old new (KVar k ds) = KVar k ((\i -> if i == old then new else i) <$> ds)
changeDom _ _ v = v



-- Collect variables referenced by step
collectReferencedVars :: ExtStep -> HS.HashSet VV
collectReferencedVars = withExtStep collectReferencedVars'

collectProducedVars :: ExtStep -> HS.HashSet VV
collectProducedVars = withExtStep collectProducedVars'

findReprForK :: Int -> ExtStep -> First ReprI
findReprForK i = withExtStep (findReprForK' i)


withExtStep :: Monoid t => (Step -> t) -> ExtStep -> t
withExtStep f = \case
  Step s      -> f s
  -- Call dh _ _ -> f (SplitStep dh [])
  _           -> mempty

findReprForK' :: Int -> Step -> First ReprI
findReprForK' i = \case
  KernelStep kb
    | kernId kb == i -> First $ Just (kernRepr kb)
    | otherwise      -> mconcat $ map fromKDep $ kernDeps kb
  _ -> mempty
  where
    fromKDep (KernelDep kid r) | i == kid  = First $ Just r
                               | otherwise = mempty

-- Collect variables referenced by step
collectReferencedVars' :: Step -> HS.HashSet VV
collectReferencedVars' = \case
  DomainStep _dh   -> mempty
  KernelStep kb    -> HS.fromList $ concat
    [ KVar kid (reprDomain repr)
    : (DVar <$> reprDomain repr)
    | KernelDep kid (ReprI repr) <- kernDeps kb
    ]
  -- singleton $ KernVar $ kernId kb
  -- SplitStep  dh ss ->
  --   let Just parD = dhParent dh
  --   in    (HS.singleton $ DVar $ dhId parD)
  --      <> foldMap collectReferencedVars' ss
  DistributeStep dh _ ss -> (HS.singleton $ DVar $ dhId dh)
                   <> foldMap collectReferencedVars' ss

-- Collect variables produced by step
collectProducedVars' :: Step -> HS.HashSet VV
collectProducedVars' = \case
  DomainStep dh -> HS.singleton $ DVar $ dhId dh
  KernelStep kb -> case kernRepr kb of
    ReprI repr -> HS.singleton $ KVar (kernId kb) (reprDomain repr)
  -- SplitStep  dh ss       -> (HS.singleton $ DVar $ dhId dh)
  --                        <> foldMap collectProducedVars' ss
  DistributeStep _ _ ss  -> foldMap collectProducedVars' ss



----------------------------------------------------------------
-- AST for DNA expression
----------------------------------------------------------------

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
  | SSplit RegSplit (StepE a)

  | SSeq  (StepE a) (StepE a)
    -- ^ Sequence two monadic actions
  | SBind (StepE a) (Scope () StepE a)
    -- ^ Monadic bind. We only introduce new variables in monadic context.

  | SActorGrp Int [StepE a]
    -- ^ Corresponds to group of actor
    --
    -- * Actor ID
    -- * Input parameters
  | SActorRecvK ReprI (StepE a) (StepE a)
    --
    -- * Representation of resulting vector
    -- * Channel variable
    -- * Region of whole vector
  | SActorRecvD (StepE a)
    -- ^ Receive data from actors
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


instance Applicative StepE where
  pure  = V
  (<*>) = ap
instance Monad StepE where
  return = V
  V a              >>= f = f a
  Pair a b         >>= f = Pair (a >>= f) (b >>= f)
  List xs          >>= f = List (map (>>= f) xs)
  SDom  i d        >>= _ = SDom i d
  SSplit s e       >>= f = SSplit s (e >>= f)
  SKern k xs ys    >>= f = SKern k (map (>>= f) xs) (map (>>= f) ys)
  SSeq  a b        >>= f = SSeq  (a >>= f) (b >>= f)
  SBind e g        >>= f = SBind (e >>= f) (g >>>= f)
  SActorGrp i xs    >>= f = SActorGrp i (map (>>= f) xs)
  SActorRecvK r a b >>= f = SActorRecvK r (a >>= f) (b >>= f)
  SActorRecvD   a   >>= f = SActorRecvD (a >>= f)



----------------------------------------------------------------
-- Compilation to expression tree
----------------------------------------------------------------

-- | Variable name
data V
  = KernVar Int
    -- ^ @Vector ()@ produced by kernel. It's referenced by KernelID
  | DomVar  Int
    -- ^ Region (referenced by DomainID)
  | ChanVar Int
  deriving (Show,Eq,Generic)
instance Hashable V

data DnaActor
  = MainActor   (StepE V)
  | RemoteActor (Scope () StepE V)


compileProgram
  :: ActorTree (ActorTy, (Vars,[ExtStep]))
  -> (DnaActor, HM.HashMap Int DnaActor)
compileProgram atree@(ActorTree a _)
  = (compile a, compile <$> flatten atree)
  where
    compile (ty,(_,steps)) = compileActor ty steps
    -- Flatten tree
    flatten :: ActorTree a -> HM.HashMap Int a
    flatten (ActorTree _ hm)
      =  fmap (\(ActorTree a _) -> a) hm
      <> mconcat (toList $ fmap flatten hm)

compileActor :: ActorTy -> [ExtStep] -> DnaActor
compileActor ty steps = case ty of
  SimpleActor     -> MainActor prog
  DistrVarActor [DVar i]   _ -> RemoteActor $ abstract1 (DomVar  i) prog
  DistrVarActor [KVar i _] _ -> RemoteActor $ abstract1 (KernVar i) prog
  DistrActor{}    -> error "DistrActor should not appear there"
  DistrVarActor{} -> error "DistrActorVar should have single var only"
  where
    prog = compileSteps steps

compileSteps :: [ExtStep] -> StepE V
compileSteps []     = error "compileSteps: empty list"
compileSteps [x]    = toStep $ singleStep x
compileSteps (x:xs) =
  let rest = compileSteps xs
  in case singleStep x of
       StepVal   expr v -> expr `SBind` abstract1 v rest
       Step2Val  e1 v1 e2 v2 ->
         e1 `SBind` abstract1 v1 (e2 `SBind` abstract1 v2 rest)
       StepNoVal expr   -> expr `SSeq` rest

data StepRes
  = StepVal   (StepE V) V
  | Step2Val  (StepE V) V (StepE V) V
  | StepNoVal (StepE V)

toStep :: StepRes -> StepE V
toStep = \case
  StepVal   e _ -> e
  StepNoVal e   -> e

singleStep :: ExtStep -> StepRes
singleStep = \case
  ----------------------------------------
  -- Single step
  Step s -> case s of
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
    _ -> error "Other steps should not appear in transformed program"
  ----------------------------------------
  -- Call actor
  Call dh pars i ->
    let dhp = case dhParent dh of
              Just d  -> d
              Nothing -> error "Parent??"
    in Step2Val
       (SSplit (RegSplit $ dhRegion dhp) (V $ DomVar $ dhId dhp))
       (DomVar $ dhId dh)
       (SActorGrp i [ case p of
                        DVar n   -> V (DomVar n)
                        KVar n _ -> V (error "A")
                    | p <- pars ])
       (ChanVar i)
  -- Gather results from vector and build full vector from it.
  Expect _ [(Nothing,DVar di)]   -> error "Expecting domain is not implemented"
  Expect i [(Just r@(ReprI repr), KVar ki _)] ->
    let [did] = reprDomain repr
    in StepVal ( SActorRecvK r
                   (V $ ChanVar i)
                   (V $ DomVar  did)
               )
               ( KernVar ki )
  Expect{} -> error "Do not support expecting more than 1 element"
  -- Yield result
  Yield [KVar ki _] -> let v = KernVar ki in StepVal (V v) v
  Yield [DVar di]   -> let v = DomVar di  in StepVal (V v) v
  Yield{} -> error "Can only yield single value"
  where
    kbDomList kb = case kernRepr kb of
      ReprI r -> V . DomVar <$> reprDomain r



----------------------------------------------------------------
-- Interpretation
----------------------------------------------------------------

data Box
  = VReg  RegionBox
  | VVec  RegionBox (Vector ())
  | VChan (DNA.Group Box)
  deriving (Typeable)

instance Bin.Binary Box where
  put = undefined
  get = undefined

interpretAST
  :: HM.HashMap Int DnaActor -> StepE V -> (RemoteTable -> RemoteTable, DNA Box)
interpretAST actorMap mainActor = case closed mainActor of
  Nothing -> error "interpretAST: expression is not closed!"
  Just e  -> (rtable,go e)
  where
    go = \case
      V a    -> return a
      Pair{} -> error "Naked pair at the top level"
      List{} -> error "Naked list at the top level"
      -- Domains
      SDom _ (NewRegion dom) ->
        DNA.kernel "dom" [] $ liftIO $ VReg . pure <$> dom
      SSplit (RegSplit split) reg ->
        case toDom reg of
          [r] -> do DNA.kernel "split" [] $ liftIO $ VReg <$> split r
          _   -> error "Non unary domain!"
      -- Kernel
      SKern kb deps dom ->
        let xs  = map toParam deps
            out = toDom =<< dom
        in DNA.kernel "kern" [] $ liftIO $ VVec out <$> kernCode kb xs out
      -- Monadic bind
      SBind expr lam ->
        let dnaE = go expr
            lamE = \a -> go $ instantiate1 (V a) lam
        in dnaE >>= lamE
      SSeq e1 e2 -> go e1 >> go e2
      -- Actor spawning
      SActorGrp actID [par] -> do
        let regs = toDom par
            n    = length regs
            (clos,_) = amap ! actID
        logMessage $ show regs
        sh  <- startGroup (N n) (NNodes 1) $ return clos
        grp <- delayGroup sh
        return $ VChan grp
      SActorGrp _ _ -> error "Only actor with one parameter are supported"
      -- Receiving of parameters
      SActorRecvK (ReprI repr) vCh vReg -> do
        let ch  = toGrp vCh
            reg = toDom vReg
        xs <- gather ch (flip (:)) []
        let pars = flip map xs $ \case
                     VVec v p -> (v,p)
                     _        -> error "Only vector expected!"
        DNA.kernel "merge" [] $ liftIO $ do
          Just vec <- reprMerge repr pars reg
          return $ VVec reg vec
        undefined
      SActorRecvD{} -> error "Receiving of domains is not implemented"
    --
    toDom = \case
      V (VReg d) -> d
      V  VVec{}  -> error "Vector where domain expected"
      _          -> error "Only variables expected"
    toParam = \case
      Pair (V (VVec _ v)) (List p) -> (v, toDom =<< p)
      _                            -> error "Ill formed parameters"
    toGrp = \case
      V (VChan ch) -> ch
      V _          -> error "Not a chan var"
      _            -> error "Only variables expected"
    --
    runActor :: DnaActor -> DNA.Actor Box Box
    runActor = \case
      MainActor   a   -> error "Not supported"
      RemoteActor lam ->
        let Just lam' = closed lam
        in  DNA.actor $ \x -> go (instantiate1 (V x) lam')
    --
    Endo rtable = foldMap (Endo . snd) amap
    amap = HM.mapWithKey (\i a -> let (clos,reg) = mkClosureValSingle ("DNA_NAME_" ++ show i) $ \_ -> a
                                  in (clos (), reg)
                         )
         $ fmap runActor actorMap



----------------------------------------------------------------
-- Pretty printer
----------------------------------------------------------------


-- | Pretty print AST
prettyprint :: Show a => StepE a -> Doc
prettyprint = flip evalState varNames . ppr . fmap Right

-- | Pretty print AST
prettyprintLam :: Show a => Scope () StepE a -> Doc
prettyprintLam lam
  = flip evalState varNames
  $ do v   <- fresh
       doc <- ppr $ instantiate1 (V (Left v)) $ fmap Right lam
       return $ vcat
         [ text ("λ"++v++" →")
         , nest 2 doc
         ]

varNames = map (:[]) ['a' .. 'z']


pprList :: Show a => [StepE (Either String a)] -> State [String] Doc
pprList es = do ss <- mapM ppr es
                return $ brackets $ hcat $ intersperse comma ss

ppr :: Show a => StepE (Either String a) -> State [String] Doc
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
  SSplit _ e -> do
    s <- ppr e
    return $ hcat [ text "SSplit {"
                  , s
                  , text "}"
                  ]
  SBind e lam -> do
    v  <- fresh
    se <- ppr e
    sl <- ppr $ instantiate1 (V $ Left v) lam
    return $ vcat
      [ se <> text (" $ λ"++v++" →")
      , nest 2 sl
      ]
  SActorGrp i vars -> do
    xs <- pprList vars
    return $ hcat [ text "Actor Grp "
                  , int i
                  , text " "
                  , xs
                  ]
  SActorRecvK r vCh vReg  -> do
    sCh  <- ppr vCh
    sReg <- ppr vReg
    return $ hcat [ text "Actor recv K "
                  , text $ " {"++(show r)++"} "
                  , sCh
                  , text " "
                  , sReg
                  ]
  SActorRecvD v -> do
    s <- ppr v
    return $ hcat [ text "Actor recv D "
                  , s
                  ]

fresh = do
  v : rest <- get
  put rest
  return v

