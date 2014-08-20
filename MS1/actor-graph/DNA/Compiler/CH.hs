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
  writeFile (dir++"/main.hs") (HS.prettyPrint m)


----------------------------------------------------------------
-- Code generation
--
-- For every node in  the dataflow graph we spawn an actor
----------------------------------------------------------------

-- | Compile graph to cloud haskell project
compileToCH :: DataflowGraph -> Compile Project
compileToCH gr = do
  -- Generate declarations for every actor
  (actorMap,allNames,actorDecls) <- compileAllNodes gr
  master                         <- buildMaster gr actorMap
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
                   (concat [ actorDecls
                           , buildRemoteTable allNames
                           , master
                           , [ HS.TypeSig loc [HS.Ident "main"] [ty| IO () |]
                             , HS.Ident "main" =: [hs| defaultMain __remoteTable master |]
                             ]
                           ]
                   )
    }

-- Compile all actor to cloud haskell expressions. Function returns
-- map from node id to pair of 1) CH node index and name of name of
-- function and 2) list of all generated declarations.
compileAllNodes :: DataflowGraph -> Compile (IntMap (ScheduleState,HS.Name), [HS.Name], [HS.Decl])
compileAllNodes gr = do
  actors <- forM (nodes gr) $ \n -> do
    let a = lab' $ context gr n
        i = case a of ANode j _ -> j
    ActorRes nm nms decls <- case a of ANode _ aa -> compileNode aa
    return (n,(i,nm,nms,decls))
  return ( IntMap.fromList [(n,(i,nm)) | (n,(i,nm,_,_)) <- actors]
         , concat [ nms  | (_,(_,_,nms,_ )) <- actors]
         , concat [ decl | (_,(_,_,_,decl)) <- actors]
         )

-- Build remote table for CH
buildRemoteTable :: [HS.Name] -> [HS.Decl]
buildRemoteTable allNames =
  [ HS.SpliceDecl loc
  $ [hs|remotable|] $$ HS.List [var (quote nm) | nm <- allNames]
  ]

-- Generate master fucntion
buildMaster :: DataflowGraph -> IntMap (ScheduleState,HS.Name) -> Compile [HS.Decl]
buildMaster gr actorMap = do
  -- Identifier for node list
  vnodes <- HS.Ident <$> fresh "nodes"
  -- Generate map from node ID to variable it's bound and statement
  -- for spawning CH process
  actorVar <- T.forM actorMap $ \(sched,nm) ->
    spawnActor vnodes sched nm
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
           -> HS.Name           -- Name of haskell function to which
                                -- actor was compiled
           -> Compile (HS.Name,[HS.Stmt])
spawnActor vnodes sched nm = do
  x <- HS.Ident <$> freshName
  i <- case sched of
         NotSched         -> compError ["Node is not scheduled for execution"]
         Single       i   -> return i
         MasterSlaves i _ -> return i
  -- Statement for spawning process
  let spawnStmt = x <-- ([hs| spawnActor |]
                         $$ infx (var vnodes) "!!" (liftHS i)
                         $$ (HS.SpliceExp $ HS.ParenSplice $ [hs|mkStaticClosure|] $$ var (quote nm))
                        )
  -- Optional sending of scheduling data
  let sendSlaves = case sched of
        MasterSlaves _ is ->
          [ stmt $ [hs|send|] $$ var x $$
             ([hs|map|] $$ HS.LeftSection (var vnodes) (HS.QVarOp (HS.UnQual (HS.Symbol "!!"))) $$ liftHS is)
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
             [(i,HVar (actorVars ! to)) | (_,to,ConnInfo (ConnId i) _) <- conns
             ]
      rmap = [hs|RemoteMap|] $$ var vme $$ tbl
  in stmt $ [hs|send|] $$ var apid $$ rmap

-- Compile actor to haskell declaration. It returns name of top level
-- declaration and list of declarations
compileNode :: RealActor -> Compile ActorRes
-- ** State machine
--
compileNode (StateM _ i rules) = do
  nm   <- HS.Ident <$> fresh "actor"  -- Name of the actor
  pids <- HS.Ident <$> fresh "pids"   -- Name of collection of remotes
  srv  <- HS.Ident <$> fresh "server"
  -- Compile rules into haskell expressions
  ruleExprs <- sequence [ compileExpr (Env pids None) r | Rule r <- rules ]
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
              , stmt $ [hs| startActor |] $$ stExpr $$ var srv
              ]
  --
  return ActorRes
    { aresName     = nm
    , aresDeclName = [nm]
    , aresDecls    = [ HS.TypeSig loc [nm] [ty| Process () |]
                     , nm =: (HS.Do exprs)
                     ]
    }
-- ** Data source
--
compileNode (Producer _ i step) = do
  -- Name of the actor
  nm <- HS.Ident <$> fresh "actor"
  -- Name of collection of remotes
  pids <- HS.Ident <$> fresh "pids"
  --
  stepExpr <- compileExpr (Env pids None) step
  stExpr   <- compileExpr (Env pids None) i
  let exprs = [ pids <-- [hs| expect :: Process RemoteMap |]
              , stmt $ [hs|producer|] $$ stExpr $$ stepExpr
              ]
  return ActorRes
    { aresName     =  nm
    , aresDeclName = [nm]
    , aresDecls    = [ HS.TypeSig loc [nm] [ty| Process () |]
                     , nm =: (HS.Do exprs)
                     ]
    }
-- ** Scatter-gather
--
compileNode (ScatterGather _ (SG (st,merge,outs) worker scatter)) = do
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
            $$ liftHS (exprSt,exprMerge,exprOut)
            $$ exprScatter
            $$ (HS.SpliceExp $ HS.ParenSplice $ [hs|mkStaticClosure|] $$ var (quote workerNm))
        ]
      exprWFun =
        [ stmt $ [hs| worker |] $$ exprWorker
        ]
  return ActorRes
    { aresName     = masterNm
    , aresDeclName = [masterNm,workerNm]
    , aresDecls    = [ HS.TypeSig loc [masterNm] [ty| Process () |]
                     , masterNm =: (HS.Do exprMaster)
                     , HS.TypeSig loc [workerNm] [ty| Process () |]
                     , workerNm =: (HS.Do exprWFun)
                     ]
    }

data ActorRes = ActorRes
  { aresName     :: HS.Name     -- Name of main actor function
  , aresDeclName :: [HS.Name]   -- Names of all declarations
  , aresDecls    :: [HS.Decl]   -- All top-level declarations
  }

----------------------------------------------------------------
-- Compilation of Expr
----------------------------------------------------------------

-- Compile AST to haskell expression
compileExpr :: Env env -> Expr env a -> Compile HS.Exp
compileExpr env@(Env pids _) expr =
  case expr of
    -- Let expression
    Let bound e -> do
      x  <- HS.Ident <$> freshName
      eb <- compileExpr env bound
      ee <- compileExpr (bind x env) e
      return $ HS.Let (HS.BDecls [ x =: eb ]) ee
    -- Bound variable
    Var idx -> return $ var $ lookupVar idx env
    -- Function application
    Ap f a -> do ef <- compileExpr env f
                 ea <- compileExpr env a
                 return $ HS.App ef ea
    -- Lambda function
    Lam f  -> do
      x  <- HS.Ident <$> freshName
      ef <- compileExpr (bind x env) f
      return $ HS.Lambda loc [HS.PatTypeSig loc (HS.PVar x) (typeOfVar (bvar f))
                             ] ef
    -- Fold
    Fold f a vec -> do ef <- compileExpr env f
                       ea <- compileExpr env a
                       ev <- compileExpr env vec
                       return $ [hs| foldArray |] $$ ef $$ ea $$ ev
    -- Zip
    Zip  f va vb -> do ef <- compileExpr env f
                       ea <- compileExpr env va
                       eb <- compileExpr env vb
                       return $ [hs| zipArray |] $$ ef $$ ea $$ eb
    -- Generate
    Generate sh f -> do esh <- compileExpr env sh
                        ef  <- compileExpr env f
                        return $ [hs| generateArray |] $$ esh $$ ef
    -- Primitives
    Add -> return $ var (HS.Symbol "+")
    Mul -> return $ var (HS.Symbol "*")
    FromInt -> return $ [hs| fromIntegral |]
    -- Scalars
    Scalar a -> return $ compileScalar a
    Tup tup  -> compileTuple env tup
    Prj idx  -> compileTupleProj idx
    String s -> return $ HS.Lit (HS.String s)
    -- List
    List xs -> return $ liftHS $ map compileScalar xs
    FMap f xs -> do ef  <- compileExpr env f
                    exs <- compileExpr env xs
                    return $ [hs|map|] $$ ef $$ exs
    -- Result expression
    Out outs -> do
      eouts <- forM outs $ \o ->
        case o of
          Outbound (ConnSimple (ConnId i)) a -> do
            ea <- compileExpr env a
            return $ [hs| sendToI |] $$ var pids $$ liftHS i $$ ea
          OutRes a -> do
            ea <- compileExpr env a
            return $ [hs| sendResult |] $$ var pids $$ ea
          PrintInt a -> do
            ea <- compileExpr env a
            return $ [hs| say . show |] $$ ea
      return $ [hs|sequence_|] $$ HS.List eouts
    -- Array sizes
    EShape sh -> return $ liftHS sh
    ESlice sl -> return $ liftHS sl
    ScatterShape -> return [hs| scatterShape |]
    --
    Vec _ -> error "NOT IMPLEMENTED"

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
compileTuple :: Env env -> Tuple (Expr env) xs -> Compile HS.Exp
compileTuple env tup
  = HS.Tuple HS.Boxed <$> sequence (compileElts env tup)
  where
    compileElts :: Env env -> Tuple (Expr env) xs -> [Compile HS.Exp]
    compileElts _ Nil = []
    compileElts e (Cons expr rest) = compileExpr e expr
                                   : compileElts e rest

compileTupleProj :: TupleIdx xs x -> Compile HS.Exp
compileTupleProj idx = do
  v <- HS.Ident <$> freshName
  return $ HS.Lambda loc [HS.PTuple HS.Boxed (wilds v idx)] (var v)
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

-- | Function application
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
x =: expr = HS.PatBind loc (HS.PVar x) Nothing (HS.UnGuardedRhs expr) (HS.BDecls [])

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

quote :: HS.Name -> HS.Name
quote (HS.Ident nm) = HS.Ident ("'" ++ nm)
quote (HS.Symbol _) = error "DNA.Compiler.CH.quote: cannot quote symbol"

-- | Infix function application
infx :: HS.Exp -> String -> HS.Exp -> HS.Exp
infx e1 op e2 = HS.InfixApp e1 (HS.QVarOp (HS.UnQual (HS.Symbol op))) e2
