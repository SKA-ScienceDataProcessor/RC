{-# LANGUAGE GADTs           #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Compile dataflow graph to cloud haskell
module DNA.Compiler.CH where

import Control.Applicative
import Control.Monad

import qualified Data.IntMap as IntMap
import           Data.IntMap   (IntMap,(!))
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

data Project = Project
  { prjName    :: String
  , prjCabal   :: String
  , prjMain    :: HS.Module
  }

saveProject :: FilePath -> Project -> IO ()
saveProject dir (Project _ _ m) = do
  writeFile (dir++"/main.hs") (HS.prettyPrint m)


----------------------------------------------------------------
-- Code generation
--
-- For every node in  the dataflow graph we spawn an actor
----------------------------------------------------------------

compileToCH :: DataflowGraph -> Compile Project
compileToCH gr = do
  -- Generate declarations for every actor
  actors <- forM (nodes gr) $ \n -> do
    (nm,decls) <- case lab' $ context gr n of
                    ANode' a -> compileNode a
    return (n,(nm,decls))
  let actorMap = IntMap.fromList [(n,nm) | (n,(nm,_)) <- actors]
  -- * Generate master process
  --
  -- Code for spawning worker processes
  actorVar <- T.forM actorMap $ \(HS.Ident bare) -> do
    x <- HS.Ident <$> freshName
    return (x, x <-- ([hs| spawn nodeID |]
                      $$ (HS.SpliceExp $ HS.ParenSplice $
                          [hs|mkStaticClosure|] $$ HS.Var (HS.UnQual (HS.Ident ("'" ++ bare))))
                      )
           )
  -- Build remote connection tables
  let tables =
        [ let conns = out gr n           -- outgoing connections
              apid  = fst $ actorVar ! n -- PID of an actor
          in stmt $ [hs| send |] $$ HS.Var (HS.UnQual apid) $$
               ([hs| IntMap.fromList |] $$ liftHS
                  [(i,HVar (fst (actorVar ! to))) | (_,to,ConnInfo i _) <- conns]
               )
        | n <- nodes gr
        ]
  --
  let master =
        [ HS.TypeSig loc [HS.Ident "master"] [ty| Process () |]
        , HS.Ident "master" =: HS.Do
          (concat [ map snd (IntMap.elems actorVar)
                  -- ,
                  , [ stmt [hs| monitor |] ]
                  , tables
                  ]
          )
        ]

  return Project
    { prjName  = ""
    , prjCabal = "CABAL"
    , prjMain  = HS.Module loc (HS.ModuleName "Main")
                   [pragmas ["TemplateHaskell"]]
                   Nothing      -- Warnings
                   Nothing      -- Export list
                   [ importD "Data.IntMap" True (Just "IntMap")
                   , importA "Control.Distributed.Process"
                   , importA "Control.Distributed.Process.Platform.ManagedProcess"
                   , importA "DNA.CH"
                   ]           -- Imports
                   (concat [ (\(_,(_,d)) -> d) =<< actors
                           , master
                           , [ HS.TypeSig loc [HS.Ident "main"] [ty| Process () |]
                             , HS.Ident "main" =: [hs| defaultMain __remoteTable master |]
                             ]
                           ]
                   )
    }

-- Compile actor for
compileNode :: Actor' outs  -> Compile (HS.Name,[HS.Decl])
compileNode (StateM outs conns i rules) = do
  -- Name of the actor
  nm <- HS.Ident <$> fresh "actor"
  -- Name of collection of remotes
  pids <- HS.Ident <$> fresh "pids"
  srv  <- HS.Ident <$> fresh "server"
  -- Compile rules into haskell expressions
  ruleExprs <- sequence [ compileExpr (Env pids None) r | Rule r <- rules ]
  -- Compile initial state into haskell expression
  stExpr    <- compileExpr (Env pids None) i
  --
  let exprs = [ pids <-- [hs| expect :: IntMap ProcessId |]
              , HS.LetStmt $ HS.BDecls
                  [ srv =: recUpd (HS.Ident "defaultProcess")
                    [ "apiHandlers" ==: HS.List
                      [ [hs| handleRule |] $$ e | e <- ruleExprs ]
                    ]
                  ]
              , stmt $ [hs| startActor |] $$ stExpr $$ HS.Var (HS.UnQual srv)
              ]
  --
  return (nm,[ HS.TypeSig loc [nm] [ty| Process () |]
             , nm =: (HS.Do exprs)
             ])


-- Compile AST to haskell expression
compileExpr :: Env env -> Expr env a -> Compile HS.Exp
compileExpr env@(Env pids _) expr =
  case expr of
    -- Let expression
    Let bound e -> do
      x  <- freshName
      eb <- compileExpr env bound
      ee <- compileExpr (bind x env) e
      return $ HS.Let (HS.BDecls [ HS.Ident x =: eb ]) ee
    -- Bound variable
    Var idx -> return $ HS.Var $ HS.UnQual $ HS.Ident $ lookupVar idx env
    -- Function application
    Ap f a -> do ef <- compileExpr env f
                 ea <- compileExpr env a
                 return $ HS.App ef ea
    -- Lambda function
    Lam f -> do x  <- freshName
                ef <- compileExpr (bind x env) f
                return $ HS.Lambda loc [HS.PVar (HS.Ident x)] ef
    -- Fold
    Fold f a vec -> do ef <- compileExpr env f
                       ea <- compileExpr env a
                       ev <- compileExpr env vec
                       return $ [hs| DNA.fodlArray |] $$ ef $$ ea $$ ev
    -- Zip
    Zip  f va vb -> do ef <- compileExpr env f
                       ea <- compileExpr env va
                       eb <- compileExpr env vb
                       return $ [hs| DNA.zipArray |] $$ ef $$ ea $$ eb
    -- Generate
    Generate sh f -> do esh <- compileExpr env sh
                        ef  <- compileExpr env f
                        return $ [hs| DNA.generateArray |] $$ esh $$ ef
    -- Primitives
    Add -> return $ HS.Var (HS.UnQual (HS.Symbol "+"))
    Mul -> return $ HS.Var (HS.UnQual (HS.Symbol "*"))
    -- Scalars
    Scalar a -> return $
      case reifyScalar a of
        DoubleDict -> liftHS a
        IntDict    -> liftHS a
        UnitDict   -> [hs| () |]
    Tup2 a b -> do
      ea <- compileExpr env a
      eb <- compileExpr env b
      return $  HS.Tuple HS.Boxed [ea, eb]
    String s -> return $ HS.Lit (HS.String s)
    -- Result expression
    Out outs -> do
      eouts <- forM outs $ \(Outbound i a) -> do
        ea <- compileExpr env a
        return $ [hs| sendToI |] $$ HS.Var (HS.UnQual pids) $$ liftHS i $$ ea
      return $ [hs|sequence|] $$ HS.List eouts
    -- Array sizes
    EShape sh -> return $ liftHS sh
    ESlice sl -> return $ liftHS sl
    --
    Vec _ -> error "NOT IMPLEMENTED"


data Env env = Env HS.Name (Ctx env)

-- Bound variables in context of expression
data Ctx env where
  None :: Ctx ()
  BVar :: String -> Ctx e -> Ctx (e,t)

lookupVar :: Idx env t -> Env env -> String
lookupVar idx (Env _ c) = go idx c
  where
    go :: Idx env t -> Ctx env -> String
    go  ZeroIdx    (BVar nm _) = nm
    go (SuccIdx i) (BVar _  v) = go i v
    go  _           _          = error "Impossible"


bind :: String -> Env env -> Env (env,t)
bind nm (Env n e) = Env n (BVar nm e)


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

class Lift a where
  liftHS :: a -> HS.Exp

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

instance Lift a => Lift [a] where
  liftHS xs = HS.List (map liftHS xs)

newtype HVar = HVar HS.Name

instance Lift HVar where
  liftHS (HVar nm) = HS.Var (HS.UnQual nm)

loc :: HS.SrcLoc
loc = HS.SrcLoc "<unknown>.hs" 1 1

($$) :: HS.Exp -> HS.Exp -> HS.Exp
($$) = HS.App

(<--) :: HS.Name -> HS.Exp -> HS.Stmt
x <-- expr = HS.Generator loc (HS.PVar x) expr

stmt :: HS.Exp -> HS.Stmt
stmt = HS.Qualifier

(=:) :: HS.Name -> HS.Exp -> HS.Decl
x =: expr = HS.PatBind loc (HS.PVar x) Nothing (HS.UnGuardedRhs expr) (HS.BDecls [])


recUpd :: HS.Name -> [HS.FieldUpdate] -> HS.Exp
recUpd nm upds = HS.RecUpdate (HS.Var (HS.UnQual nm)) upds

(==:) :: String -> HS.Exp -> HS.FieldUpdate
nm ==: expr = HS.FieldUpdate (HS.UnQual (HS.Ident nm)) expr

pragmas :: [String] -> HS.ModulePragma
pragmas = HS.LanguagePragma loc . map HS.Ident

importD :: String -> Bool -> Maybe String -> HS.ImportDecl
importD modN qual as
  = HS.ImportDecl loc (HS.ModuleName modN) qual False Nothing (HS.ModuleName <$> as) Nothing

importA modN = importD modN False Nothing
