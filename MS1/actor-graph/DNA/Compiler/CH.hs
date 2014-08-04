{-# LANGUAGE GADTs           #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Compile dataflow graph to cloud haskell
module DNA.Compiler.CH where

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
  , prjMain    :: String
  }



----------------------------------------------------------------
-- Code generation
--
-- For every node in  the dataflow graph we spawn an actor
----------------------------------------------------------------

compileToCH :: DataflowGraph -> Compile Project
compileToCH gr = do
  undefined
  where


compileNode :: Actor' outs  -> Compile Project
compileNode (StateM outs conns i rules) = do
  undefined


compileExpr :: Context env -> Expr env a -> Compile HS.Exp
compileExpr env expr =
  case expr of
    -- Let expression
    Let bound e -> do
      x  <- freshName
      eb <- compileExpr env bound
      ee <- compileExpr (BVar x env) e
      return $ HS.Let (HS.BDecls
                         [ HS.PatBind loc (HS.PVar (HS.Ident x)) Nothing (HS.UnGuardedRhs eb) (HS.BDecls [])
                         ]
                      ) ee
    -- Bound variable
    Var idx -> return $ HS.Var $ HS.UnQual $ HS.Ident $ boundVar idx env
    -- Function application
    Ap f a -> do ef <- compileExpr env f
                 ea <- compileExpr env a
                 return $ HS.App ef ea
    -- Lambda function
    Lam f -> do x  <- freshName
                ef <- compileExpr (BVar x env) f
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
    Out   os -> error "NOT IMPLEMENTED"
    -- Array sizes
    EShape sh -> return $ liftHS sh
    ESlice sl -> return $ liftHS sl
    -- 
    Vec _ -> error "NOT IMPLEMENTED" 


data Context env where
  None :: Context ()
  BVar :: String -> Context e -> Context (e,t)

boundVar :: Idx env t -> Context env -> String
boundVar  ZeroIdx    (BVar nm _) = nm
boundVar (SuccIdx i) (BVar _  v) = boundVar i v
boundVar _ _ = error "Impossible"


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


loc :: HS.SrcLoc
loc = HS.SrcLoc "<unknown>.hs" 1 1

($$) = HS.App
