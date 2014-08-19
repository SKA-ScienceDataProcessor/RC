{-# LANGUAGE GADTs #-}
-- | Implementation of distributed dot product
import DNA.Actor
import DNA.AST
import DNA.Compiler.CH
import DNA.Compiler.Scheduler
import DNA.Compiler.Types


----------------------------------------------------------------
-- Simple dot product actor
----------------------------------------------------------------

-- | Simple dot product actor. It doesn't have any parallelism.
--
--   > \() shape -> ((), result (fold (+) 0 $ zip (*) (generate fromIntegral shape)
--                                                    (generate fromIntegral shape)))
simpleDotProduct :: Expr () (() -> Shape -> ((),Out))
simpleDotProduct = Lam $ Lam $ Tup
  (      Scalar ()
  `Cons` Out [OutRes $ Fold Add (Scalar (0::Double)) (Zip Mul
                                                       (Generate (Var ZeroIdx) FromInt)
                                                       (Generate (Var ZeroIdx) FromInt)
                                                     )
             ]
  `Cons` Nil
  )



----------------------------------------------------------------
-- Explicitly parallel version of ddp
----------------------------------------------------------------

ddpGather :: (Expr () Double, Expr () (Double -> Double -> Double), Expr () (Double -> Out))
ddpGather =
  ( Scalar 0
  , Add
  , Lam (Out [OutRes (Var ZeroIdx)])
  )

ddpWorker :: Expr () (Slice -> Double)
ddpWorker = Lam $
  Fold Add (Scalar 0) $
    Zip Mul
      (Generate (Var ZeroIdx) FromInt)
      (Generate (Var ZeroIdx) FromInt)

ddpScatter :: Expr () (Int -> Shape -> [Slice])
ddpScatter = ScatterShape

-- | Very hacky producer of shape
shapeProducer :: ConnSimple Shape -> Shape -> Expr () (() -> ((),Out))
shapeProducer conn sh
  = Lam $ Tup (Scalar () `Cons` Out [Outbound conn (EShape sh)] `Cons` Nil)



----------------------------------------------------------------
-- Simple dot product dataflow
----------------------------------------------------------------

simpleDataflow = do
  -- Dot product actor
  (aDot, ()) <- use $ actor $ do
    startingState $ Scalar ()
    rule simpleDotProduct
  (_, c) <- use $ actor $ do
    c <- simpleOut
    producer $ shapeProducer c (Shape 100)
    startingState $ Scalar ()
    return c
  connect c aDot

distributedDataflow = do
  (aDot, ()) <- use $ actor $ do
    scatterGather $ SG ddpGather ddpWorker ddpScatter
  (_, c) <- use $ actor $ do
    c <- simpleOut
    producer $ shapeProducer c (Shape 100)
    startingState $ Scalar ()
    return c
  connect c aDot
  


main :: IO ()
main = do
  let r = compile $ compileToCH =<< schedule 2 =<< buildDataflow distributedDataflow
  -- let r = compile $ compileToCH =<< schedule 2 =<< buildDataflow simpleDataflow
  case r of
    Left errs -> mapM_ putStrLn errs
    Right prj -> saveProject "dir" prj

  
