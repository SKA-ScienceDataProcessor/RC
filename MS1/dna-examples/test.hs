{-# LANGUAGE GADTs #-}
import DNA.Actor
import DNA.AST
import DNA
import DNA.Compiler.CH

----------------------------------------------------------------
-- Print actor
----------------------------------------------------------------

exprPrint :: Expr () (() -> Int -> ((),Out))
exprPrint = Lam $ Lam $ Tup ( Scalar ()
                       `Cons` Out [PrintInt $ Var ZeroIdx]
                       `Cons` Nil)

actorPrint :: Actor ()
actorPrint = actor $ do
  rule $ exprPrint
  startingState $ Scalar ()
  return ()



----------------------------------------------------------------
-- Source actor
----------------------------------------------------------------

exprSrcInt :: Conn Int -> Expr () (Int -> (Int,Out))
exprSrcInt i
  = Lam $ Tup ( Ap (Ap Add (Var ZeroIdx)) (Scalar (1 :: Int))
         `Cons` Out [Outbound i (Var ZeroIdx)]
         `Cons` Nil)

actorSrcInt :: Actor (Conn Int)
actorSrcInt = actor $ do
  c <- simpleOut ConnOne
  producer $ exprSrcInt c
  startingState $ Scalar 0
  return c


program = do
  (aPr, ()) <- use actorPrint
  (_  , c ) <- use actorSrcInt
  connect c aPr

main :: IO ()
main = compile compileToCH (saveProject "dir") 2 program


