{-# LANGUAGE GADTs #-}
import DNA.Actor
import DNA.AST
import DNA.Compiler.CH
import DNA.Compiler.Scheduler
import DNA.Compiler.Types

----------------------------------------------------------------
-- Print actor
----------------------------------------------------------------

exprPrint :: Expr () (() -> Int -> ((),Out))
exprPrint = Lam $ Lam $ Tup2 (Scalar ()) (Out [PrintInt $ Var ZeroIdx])

actorPrint :: Actor ()
actorPrint = actor $ do
  rule $ exprPrint
  startingState $ Scalar ()
  return ()



----------------------------------------------------------------
-- Source actor
----------------------------------------------------------------

exprSrcInt :: ConnSimple Int -> Expr () (Int -> (Int,Out))
exprSrcInt i
  = Lam $ Tup2 (Ap (Ap Add (Var ZeroIdx)) (Scalar (1 :: Int)))
               (Out [Outbound i (Var ZeroIdx)])

actorSrcInt :: Actor (ConnSimple Int)
actorSrcInt = actor $ do
  c <- simpleOut
  producer $ exprSrcInt c
  startingState $ Scalar 0
  return c


program = do
  (aPr, ()) <- use actorPrint
  (_  , c ) <- use actorSrcInt
  connect c aPr

main :: IO ()
main = do
  let r = compile $ compileToCH =<< buildDataflow program
  case r of
    Left errs -> mapM_ putStrLn errs
    Right prj -> saveProject "dir" prj

