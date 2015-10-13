{-# LANGUAGE GADTs #-}
-- |
-- Compilation @[Step] → AST@
module Step where

import qualified Data.HashMap.Strict as HM
import Bound

import Flow
import AST

{-
type DataMap   = IM.IntMap (ReprI, Map.Map [Domain] (Vector ()))
type DomainMap = IM.IntMap (AnyDH, [Domain])
-}

-- |
-- 
data StepE a
  = Var a
  | Pair (StepE a) (StepE a)
  | List [StepE a]
    
  | SKern KernelBind [StepE a]         -- Kernel call
  | SBind (StepE a) (Scope () StepE a) -- Monadic bind

data Var
  = KernVar Int
  | DomVar  Int


compileSteps :: [Step] -> Compiler (StepE Var)
compileSteps steps = do
  -- 1. 
  undefined


toDnaAst :: StepE a -> Exp a
toDnaAst = undefined
