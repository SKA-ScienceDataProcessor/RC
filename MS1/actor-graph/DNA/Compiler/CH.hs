{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Compile dataflow graph to cloud haskell
module DNA.Compiler.CH (
    Project(..)
  , saveProject
  , compileToCH
  ) where

import Control.Applicative
import Control.Monad

import Data.Typeable
import qualified Data.IntMap as IntMap
import           Data.IntMap   (IntMap,(!))
import qualified Data.Foldable    as T
import qualified Data.Traversable as T
import Data.Graph.Inductive.Graph

import qualified Language.Haskell.Exts        as HS
import           Language.Haskell.Exts.QQ

import DNA.Compiler.Types
import DNA.AST
import DNA.Actor

import Paths_dna (getDataDir)


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Haskell project
data Project = Project
  { prjName    :: String
  , prjCabal   :: String
  , prjMain    :: HS.Module
  }

-- | Save project on disk
saveProject :: FilePath -> Project -> IO ()
saveProject dir (Project _ _ m) = do
  ddir  <- getDataDir
  cabal <- readFile (ddir ++ "/data/template.cabal")
  writeFile (dir++"/main.hs")   (HS.prettyPrint m)
  writeFile (dir++"/main.cabal") cabal

----------------------------------------------------------------
-- Code generation
--
-- For every node in  the dataflow graph we spawn an actor
----------------------------------------------------------------

-- | Compile graph to cloud haskell project
compileToCH :: DataflowGraph -> Compile Project
compileToCH gr = do
  -- Generate declarations for every actor
  actors <- compileAllNodes gr
  master <- buildMaster gr actors
  -- Assemble project
  return Project
    { prjName  = ""
    , prjCabal = "CABAL"
    , prjMain  = HS.Module loc (HS.ModuleName "Main")
                   [pragmas [ "TemplateHaskell"
                            , "ScopedTypeVariables"
                            ]]
                   Nothing      -- Warnings
                   Nothing      -- Export list
                   [ importD "Data.IntMap" True (Just "IntMap")
                   , importA "Control.Distributed.Process"
                   , importA "Control.Distributed.Process.Closure"
                   , importA "Control.Distributed.Process.Platform.ManagedProcess"
                   , importA "DNA.CH"
                   ]           -- Imports
                   (concat [ aresDecls =<< T.toList actors
                           , buildRemoteTable $ aresDeclName =<< T.toList actors
                           , master
                           , [ HS.TypeSig loc [HS.Ident "main"] [ty| IO () |]
                             , HS.Ident "main" =: [hs| defaultMain __remoteTable master |]
                             ]
                           ]
                   )
    }

-- Compile all actors to cloud haskell expressions.
compileAllNodes :: DataflowGraph -> Compile (IntMap ActorRes)
compileAllNodes gr = do
  actors <- forM (nodes gr) $ \n -> do
    let a = lab' $ context gr n
        i = case a of ANode j _ -> j
    ares <- case a of ANode _ (RealActor _ aa) -> compileNode aa
    return (n, ares)
  return $ IntMap.fromList actors


-- Build remote table for CH
buildRemoteTable :: [HS.Name] -> [HS.Decl]
buildRemoteTable allNames =
  [ HS.SpliceDecl loc
  $ [hs|remotable|] $$ HS.List [var (quote nm) | nm <- allNames]
  ]

-- Generate master fucntion
buildMaster :: DataflowGraph -> IntMap ActorRes -> Compile [HS.Decl]
buildMaster gr actorMap = do
  -- Identifier for node list
  vnodes <- HS.Ident <$> fresh "nodes"
  -- Generate map from node ID to variable it's bound and statement
  -- for spawning CH process
  actorVar <- forIntM actorMap $ \n a -> do
    let Just (ANode sch _) = lab gr n
    spawnActor vnodes sch a
  -- Variable holding PID of master process
  vme <- HS.Ident <$> fresh "me"
  -- Generate 'master' function
  return
    [ HS.TypeSig loc [HS.Ident "master"] [ty| [NodeId] -> Process () |]
    , HS.FunBind
      [ HS.Match loc (HS.Ident "master")
         [HS.PVar vnodes]
         Nothing
         (HS.UnGuardedRhs $ HS.Do
            (concat [ [vme <-- [hs| getSelfPid |]]
                    , snd =<< (IntMap.elems actorVar)
                    , [ sendConnTable gr vme (fst <$> actorVar) n | n <- nodes gr ]
                    , [ stmt [hs| monitorActors |] ]
                    ]
            ))
         (HS.BDecls [])
      ]
    ]

-- Generate code for spawning actors. Return pair of variable holding
-- PID of process and statement for do block
spawnActor :: HS.Name           -- Name of variable holding list of nodes
           -> ScheduleState     -- How process is scheduled
           -> ActorRes          -- Actor
           -> Compile (HS.Name,[HS.Stmt])
spawnActor vnodes sched actor = do
  x <- HS.Ident <$> freshName
  i <- case sched of
         NotSched         -> compError ["Node is not scheduled for execution"]
         Single       i   -> return i
         MasterSlaves i _ -> return i
  -- Statement for spawning process
  let spawnStmt = x <-- ([hs| spawnActor |]
                         $$ infx (var vnodes) "!!" (liftHS i)
                         $$ (HS.SpliceExp $ HS.ParenSplice $ [hs|mkStaticClosure|] $$ var (quote (aresName actor)))
                        )
  -- Optional sending of scheduling data
  let sendSlaves = case sched of
        MasterSlaves _ is ->
          ( stmt $ [hs|send|] $$ var x $$
             ([hs|map|] $$ HS.LeftSection (var vnodes) (HS.QVarOp (HS.UnQual (HS.Symbol "!!"))) $$ liftHS is))
          : [ stmt $ [hs|send|] $$ var x $$
                (HS.SpliceExp $ HS.ParenSplice $ [hs|mkStaticClosure|] $$ var (quote nm))
            | nm <- aresClosures actor
            ]
        _                 -> []
  return (x, spawnStmt : sendSlaves )

-- Send connection table
sendConnTable :: DataflowGraph  -- Full graph
              -> HS.Name        -- Variable holding master's process PID
              -> IntMap HS.Name -- Names of
              -> Node           -- Actor for which we generate node
              -> HS.Stmt
sendConnTable gr vme actorVars n =
  let conns = out gr n           -- outgoing connections
      apid  = actorVars ! n       -- PID of an actor
      tbl  = [hs| IntMap.fromList |] $$ liftHS
             [(i,HVar (actorVars ! to)) | (_,to,ConnId i) <- conns
             ]
      rmap = [hs|RemoteMap|] $$ var vme $$ tbl
  in stmt $ [hs|send|] $$ var apid $$ rmap

-- Compile actor to haskell declaration. It returns name of top level
-- declaration and list of declarations
compileNode :: ActorDescr -> Compile ActorRes
-- ** State machine
--
compileNode (StateM i rules) = do
  nm   <- HS.Ident <$> fresh "actor"  -- Name of the actor
  pids <- HS.Ident <$> fresh "pids"   -- Name of collection of remotes
  srv  <- HS.Ident <$> fresh "server"
  -- Compile rules into haskell expressions.
  ruleExprs <- forM rules $ \(Rule r) -> do
    codeBlock <$> compileExpr (Env pids None) r
  -- Compile initial state into haskell expression
  stExpr    <- compileExpr (Env pids None) i
  --
  let exprs = [ pids <-- [hs| expect :: Process RemoteMap |]
              , HS.LetStmt $ HS.BDecls
                  [ srv =: recUpd (HS.Ident "defaultProcess")
                    [ "apiHandlers" ==: HS.List
                      [ [hs| handleRule |] $$ e | e <- ruleExprs ]
                    ]
                  ]
              , stmt $ [hs| startActor |] $$ codeBlock stExpr $$ var srv
              ]
  --
  return ActorRes
    { aresName     = nm
    , aresDeclName = [nm]
    , aresClosures = []
    , aresDecls    = [ HS.TypeSig loc [nm] [ty| Process () |]
                     , nm =: (HS.Do exprs)
                     ]
    }
-- ** Data source
--
compileNode (Producer i step) = do
  -- Name of the actor
  nm <- HS.Ident <$> fresh "actor"
  -- Name of collection of remotes
  pids <- HS.Ident <$> fresh "pids"
  --
  stepExpr <- compileExpr (Env pids None) step
  stExpr   <- compileExpr (Env pids None) i
  let exprs = [ pids <-- [hs| expect :: Process RemoteMap |]
              , stmt $ [hs|producer|] $$ codeBlock stExpr $$ codeBlock stepExpr
              ]
  return ActorRes
    { aresName     =  nm
    , aresDeclName = [nm]
    , aresClosures = []
    , aresDecls    = [ HS.TypeSig loc [nm] [ty| Process () |]
                     , nm =: (HS.Do exprs)
                     ]
    }
-- ** Scatter-gather
--
compileNode (ScatterGather (SG (st,merge,outs) worker scatter)) = do
  masterNm <- HS.Ident <$> fresh "actor"
  workerNm <- HS.Ident <$> fresh "actor"
  pids     <- HS.Ident <$> fresh "pids"
  -- Compile expressions
  exprSt      <- compileExpr (Env pids None) st
  exprMerge   <- compileExpr (Env pids None) merge
  exprOut     <- compileExpr (Env pids None) outs
  exprWorker  <- compileExpr (Env pids None) worker
  exprScatter <- compileExpr (Env pids None) scatter
  -- Do-block for master process
  let exprMaster =
        [ pids <-- [hs| expect :: Process RemoteMap |]
        , stmt $ [hs| scatterGather |]
            $$ liftHS (codeBlock exprSt, codeBlock exprMerge, codeBlock exprOut)
            $$ codeBlock exprScatter
        ]
      exprWFun =
        [ stmt $ [hs| worker |] $$ codeBlock exprWorker
        ]
  return ActorRes
    { aresName     = masterNm
    , aresDeclName = [masterNm,workerNm]
    , aresClosures = [workerNm]
    , aresDecls    = [ HS.TypeSig loc [masterNm] [ty| Process () |]
                     , masterNm =: (HS.Do exprMaster)
                     , HS.TypeSig loc [workerNm] [ty| Process () |]
                     , workerNm =: (HS.Do exprWFun)
                     ]
    }

data ActorRes = ActorRes
  { aresName     :: HS.Name     -- Name of main actor function
  , aresDeclName :: [HS.Name]   -- Names of all declarations
  , aresClosures :: [HS.Name]   -- List of all closures to be sent to master process
  , aresDecls    :: [HS.Decl]   -- All top-level declarations
  }


----------------------------------------------------------------
-- Compilation of expressions
----------------------------------------------------------------

-- | Compiled expression. It could be either pure expression or
--   monadic one in the 'Process' monad.
--
--   Types of corresponding haskell expressions are a bit tricky:
--
-- > typeof (a → b) = a → typeof b
-- > typeof  a      = Process a
data CodeBlock
  = Pure  HS.Exp
    -- ^ Pure expression
  | Lambda HS.Pat CodeBlock
    -- ^ Lambda expression
  | Do [Stmt] HS.Exp
    -- ^ Do-block. Note that last expression must be pure.
  deriving (Show)

-- | Statement in do block
data Stmt
  = Bind HS.Pat HS.Exp         -- ^ Variable binding in do block
  | LetB HS.Pat HS.Exp         -- ^ Let binding in do block
  deriving (Show)

-- | Convert do block to haskell expression. It will have type Process a
codeBlock :: CodeBlock -> HS.Exp
codeBlock (Lambda pat f)  = HS.Lambda loc [pat] (codeBlock f)
codeBlock (Pure a)        = [hs| return |] $$ a
codeBlock (Do stmts a) = HS.Do $
  map cnv stmts ++ [stmt $ [hs| return |] $$ a]
  where
    cnv (Bind p e) = HS.Generator loc p e
    cnv (LetB p e) = HS.LetStmt $ HS.BDecls [ p =:: e ]

-- | Application for code blocks.
($$$) :: CodeBlock -> CodeBlock -> CodeBlock
-- FIXME: Let convert lambda expression to Pure and pray
e          $$$ Lambda p f
  = e $$$ Pure (HS.Lambda loc [p] (toPure f))
  where
    toPure (Pure a)     = a
    toPure (Lambda q g) = HS.Lambda loc [q] (toPure g)
    toPure _            = error "Cannot do it"
Lambda p f $$$ Pure a     = floatDownPure p a f
Lambda p f $$$ Do stmts a = floatDownMonadic p stmts a f
Pure a     $$$ Pure b     = Pure (a $$ b)
Pure a     $$$ Do stmts b = Do stmts (a $$ b)
Do stmts a $$$ Pure b     = Do stmts (a $$ b)
Do stmtA a $$$ Do stmtB b = Do (stmtA ++ stmtB) (a $$ b)

floatDownPure :: HS.Pat -> HS.Exp -> CodeBlock -> CodeBlock
floatDownPure p e cb =
  case cb of
    Do st b -> Do ( LetB p e : st ) b
    Pure  a -> Pure $ HS.Let (HS.BDecls [ p =:: e ]) a
    Lambda p' f -> Lambda p' (floatDownPure p e f)

floatDownMonadic :: HS.Pat -> [Stmt] -> HS.Exp -> CodeBlock -> CodeBlock
floatDownMonadic p stmts expr cb =
  case cb of
    Do st b -> Do (stmts ++ [LetB p expr] ++ st ) b
    Pure  a -> Do (stmts ++ [LetB p expr]) a
    Lambda p' f -> Lambda p' (floatDownMonadic p stmts expr f)

seqDoBlocks :: [CodeBlock] -> ([Stmt],[HS.Exp])
seqDoBlocks blocks =
  ( foldr go  [] blocks
  , foldr pur [] blocks
  )
  where
    pur (Lambda _ _) = error "Should not appear"
    pur (Pure a) = (a : )
    pur (Do _ a) = (a : )
    go (Lambda _ _) _ = error "Should not appear"
    go (Pure  _) xs = xs
    go (Do st _) xs = st ++ xs

seqDoBlocks_ :: [CodeBlock] -> CodeBlock
seqDoBlocks_ blocks = Do (foldr go [] blocks) (liftHS ())
  where
    go (Pure  _) xs = xs
    go (Do st _) xs = st ++ xs
    go _         _  = error "Should not appear"

-- Compile AST to haskell expression
compileExpr :: Env env -> Expr env a -> Compile CodeBlock
compileExpr env@(Env pids _) expr =
  case expr of
    -- Let expression
    Let bound ex -> do
      x  <- HS.Ident <$> freshName
      eb <- compileExpr env bound
      ee <- compileExpr (bind x env) ex
      case (eb,ee) of
        (Pure   b, Pure   e) -> return $ Pure $ HS.Let (HS.BDecls [ x =: b ]) e
        (Do st  b, Pure   e) -> return $ Do (st ++ [LetB (pvar x) b]) e
        (Pure   b, Do st  e) -> return $ Do ([LetB (pvar x) b] ++ st) e
        (Do stA b, Do stB e) -> return $ Do (stA ++ [LetB (pvar x) b] ++ stB) e
    -- Bound variable
    Var idx -> return $ Pure $ var $ lookupVar idx env
    -- Function application
    Ap f a -> do ef <- compileExpr env f
                 ea <- compileExpr env a
                 return $ ef $$$ ea
    -- Lambda function
    Lam f  -> do
      x  <- HS.Ident <$> freshName
      ef <- compileExpr (bind x env) f
      return $ Lambda (HS.PatTypeSig loc (HS.PVar x) (typeOfVar (bvar f))) ef
    -- Fold
    Fold f a vec -> do ef <- compileExpr env f
                       ea <- compileExpr env a
                       ev <- compileExpr env vec
                       return $ Pure [hs| foldArray |] $$$ ef $$$ ea $$$ ev
    -- Zip
    Zip  f va vb -> do ef <- compileExpr env f
                       ea <- compileExpr env va
                       eb <- compileExpr env vb
                       return $ Pure [hs| zipArray |] $$$ ef $$$ ea $$$ eb
    -- Generate
    Generate sh f -> do esh <- compileExpr env sh
                        ef  <- compileExpr env f
                        let gen = case reifyShape (typeOfExpr sh) of
                                    ShShape -> [hs| generateArrayShape |]
                                    ShSlice -> [hs| generateArraySlice |]
                        return $ Pure gen $$$ esh $$$ ef
    -- Primitives
    Add -> do x <- HS.Ident <$> freshName
              y <- HS.Ident <$> freshName
              return $ Lambda (HS.PVar x) $ Lambda (HS.PVar y) $ Pure $ infx (var x) "+" (var y)
    Mul -> do x <- HS.Ident <$> freshName
              y <- HS.Ident <$> freshName
              return $ Lambda (HS.PVar x) $ Lambda (HS.PVar y) $ Pure $ infx (var x) "*" (var y)
    FromInt -> return $ Pure $ [hs| fromIntegral |]
    -- Scalars
    Scalar a -> return $ Pure $ compileScalar a
    Tup tup  -> compileTuple env tup
    Prj idx  -> compileTupleProj idx
    String s -> return $ Pure $ HS.Lit (HS.String s)
    -- List
    List xs -> return $ Pure $ liftHS $ map compileScalar xs
    FMap f xs -> do ef  <- compileExpr env f
                    exs <- compileExpr env xs
                    return $ Pure [hs|map|] $$$ ef $$$ exs
    -- Result expression
    Out outs -> do
      eouts <- forM outs $ \o ->
        case o of
          Outbound (Conn (ConnId i) _) a -> do
            sendExpression ([hs| sendToI |] $$ var pids $$ liftHS i) env a
          OutRes a -> do
            sendExpression ([hs| sendResult |] $$ var pids) env a
          PrintInt a -> do
            sendExpression ([hs| say . show |]) env a
      return $ seqDoBlocks_ eouts
    -- Array sizes
    EShape sh -> return $ Pure $ liftHS sh
    ESlice sl -> return $ Pure $ liftHS sl
    ScatterShape -> do x <- HS.Ident <$> freshName
                       y <- HS.Ident <$> freshName
                       return $ Lambda (pvar x) $ Lambda (pvar y) $ Pure $ [hs| scatterShape |] $$ var x $$ var y
    --
    Vec _ -> error "NOT IMPLEMENTED"

-- Generate expression for sending messages
sendExpression :: HS.Exp -> Env env -> Expr env a -> Compile CodeBlock
sendExpression sendE env exprA = do
  ea <- compileExpr env exprA
  x  <- HS.Ident <$> freshName
  case ea of
    Lambda _ _ -> error "Internal error. Cannot send function expressions"
    Pure  a -> return $ Do [Bind (pvar x) (sendE $$ a)] (var x)
    Do st a -> return $ Do (st ++ [Bind (pvar x) (sendE $$ a)]) (var x)
    
-- | Compile scalar expression
compileScalar :: IsScalar a => a -> HS.Exp
compileScalar a
  = HS.ExpTypeSig loc expr ety
  where
    ety  = case reifyScalar a of
             DoubleDict -> [ty| Double |]
             IntDict    -> [ty| Int    |]
             UnitDict   -> [ty| ()     |]
    expr = case reifyScalar a of
             DoubleDict -> liftHS a
             IntDict    -> liftHS a
             UnitDict   -> [hs| () |]

-- | Compile tuple expression
compileTuple :: Env env -> Tuple (Expr env) xs -> Compile CodeBlock
compileTuple env tup = do
  blocks <- sequence $ compileElts env tup
  case seqDoBlocks blocks of
    ([],as) -> return $ Pure  $ HS.Tuple HS.Boxed as
    (ss,as) -> return $ Do ss $ HS.Tuple HS.Boxed as
  where
    compileElts :: Env env -> Tuple (Expr env) xs -> [Compile CodeBlock]
    compileElts _ Nil = []
    compileElts e (Cons expr rest) = compileExpr e expr
                                   : compileElts e rest

compileTupleProj :: TupleIdx xs x -> Compile CodeBlock
compileTupleProj idx = do
  v <- HS.Ident <$> freshName
  return $ Lambda (HS.PTuple HS.Boxed (wilds v idx)) (Pure $ var v)
  where
    wilds :: forall x xs. HS.Name -> TupleIdx xs x -> [HS.Pat]
    wilds v Here      = (HS.PVar v) : replicate (arity (Proxy :: Proxy xs) - 1) HS.PWildCard
    wilds v (There i) = HS.PWildCard : wilds v i


bvar :: Expr (env,a) b -> a
bvar _ = error "DNA.Compiler.CH.bvar: impossible happened"

-- | Get type of haskell scalar variable
typeOfVar :: IsValue a => a -> HS.Type
typeOfVar a =
  case reifyValue a of
    ValScalar -> case reifyScalar a of
                   DoubleDict -> [ty| Double |]
                   IntDict    -> [ty| Int |]
                   UnitDict   -> [ty| () |]
    ValShape  -> case reifyShape a of
                   ShShape -> [ty| Shape |]
                   ShSlice -> [ty| Slice |]
    ValVec -> error "Not implemented"

data Env env = Env HS.Name (Ctx env)

-- | Bound variables in context of expression
data Ctx env where
  None :: Ctx ()
  BVar :: HS.Name -> Ctx e -> Ctx (e,t)

-- | Find variable name
lookupVar :: Idx env t -> Env env -> HS.Name
lookupVar idx (Env _ c) = go idx c
  where
    go :: Idx env t -> Ctx env -> HS.Name
    go  ZeroIdx    (BVar nm _) = nm
    go (SuccIdx i) (BVar _  v) = go i v
    go  _           _          = error "Impossible"

-- | Bind variable
bind :: HS.Name -> Env env -> Env (env,t)
bind nm (Env n e) = Env n (BVar nm e)


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Lift value to the syntax of haskell
class Lift a where
  liftHS :: a -> HS.Exp

instance Lift HS.Exp where
  liftHS = id

instance Lift Int where
  liftHS = HS.Lit . HS.Int . fromIntegral

instance Lift Double where
  liftHS = HS.Lit . HS.Frac . realToFrac

instance Lift () where
  liftHS () = [hs| () |]

instance Lift Shape where
  liftHS (Shape i) = [hs| Shape |] `HS.App` liftHS i

instance Lift Slice where
  liftHS (Slice i j) = [hs| Slice |] `HS.App` liftHS i `HS.App` liftHS j

instance (Lift a, Lift b) => Lift (a,b) where
  liftHS (a,b) = HS.Tuple HS.Boxed [liftHS a, liftHS b]

instance (Lift a, Lift b, Lift c) => Lift (a,b,c) where
  liftHS (a,b,c) = HS.Tuple HS.Boxed [liftHS a, liftHS b, liftHS c]

instance Lift a => Lift [a] where
  liftHS xs = HS.List (map liftHS xs)

-- | Haskell variable
newtype HVar = HVar HS.Name

instance Lift HVar where
  liftHS (HVar nm) = var nm

-- | Unknown source location
loc :: HS.SrcLoc
loc = HS.SrcLoc "<unknown>.hs" 1 1

-- | Function application. Here we treat lambda specially
($$) :: HS.Exp -> HS.Exp -> HS.Exp
($$) = HS.App

-- | Bind in do block
(<--) :: HS.Name -> HS.Exp -> HS.Stmt
x <-- expr = HS.Generator loc (HS.PVar x) expr

-- | Statement in do block
stmt :: HS.Exp -> HS.Stmt
stmt = HS.Qualifier

-- | Bind variable.  @x := expr@ translates to declaration @x = $expr@
(=:) :: HS.Name -> HS.Exp -> HS.Decl
x =: expr = HS.PVar x =:: expr

-- | Bind pattern.  @x := expr@ translates to declaration @x = $expr@
(=::) :: HS.Pat -> HS.Exp -> HS.Decl
p =:: expr = HS.PatBind loc p Nothing (HS.UnGuardedRhs expr) (HS.BDecls [])

-- | Record update
recUpd :: HS.Name -> [HS.FieldUpdate] -> HS.Exp
recUpd nm upds = HS.RecUpdate (var nm) upds

-- | Field update
(==:) :: String -> HS.Exp -> HS.FieldUpdate
nm ==: expr = HS.FieldUpdate (HS.UnQual (HS.Ident nm)) expr

-- | List of LANGUAGE pragmas
pragmas :: [String] -> HS.ModulePragma
pragmas = HS.LanguagePragma loc . map HS.Ident

-- | General import statement
importD :: String -> Bool -> Maybe String -> HS.ImportDecl
importD modN qual as
  = HS.ImportDecl loc (HS.ModuleName modN) qual False Nothing (HS.ModuleName <$> as) Nothing

-- | Unqualified import
importA :: String -> HS.ImportDecl
importA modN = importD modN False Nothing

-- | Unqualified variable name
var :: HS.Name -> HS.Exp
var = HS.Var . HS.UnQual

-- | Pattern variable
pvar :: HS.Name -> HS.Pat
pvar = HS.PVar

quote :: HS.Name -> HS.Name
quote (HS.Ident nm) = HS.Ident ("'" ++ nm)
quote (HS.Symbol _) = error "DNA.Compiler.CH.quote: cannot quote symbol"

-- | Infix function application
infx :: HS.Exp -> String -> HS.Exp -> HS.Exp
infx e1 op e2 = HS.InfixApp e1 (HS.QVarOp (HS.UnQual (HS.Symbol op))) e2



forIntM :: Monad m => IntMap a -> (Int -> a -> m b) -> m (IntMap b)
forIntM xs f = do
  ys <- forM (IntMap.toList xs) (\(i,a) -> do{b <- f i a; return (i,b)})
  return $ IntMap.fromList ys
