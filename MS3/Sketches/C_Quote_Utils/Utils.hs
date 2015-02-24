{-# LANGUAGE
      QuasiQuotes
    , PackageImports
    , GADTs
  #-}

module Utils where

import Text.Printf (printf)
-- Let's start to migrate to FTP right now!
import Data.Foldable (foldl')

import Data.Loc (noLoc)
import "language-c-quote" Language.C.Syntax
import Language.C.Quote.CUDA
import Text.PrettyPrint.Mainland(
    Doc
  , ppr
  -- , pretty
  )

-- We relax loop dimensions types to be of any of ToExp class
-- and not only Int's here.
data LB where
  LB :: (ToExp a, ToExp b, ToExp c) => a -> b -> c -> LB

type LoopType = ([Exp] -> [Stm]) -> Stm

loopstep :: [Stm] -> (LB, Id) -> [Stm]
loopstep code (LB ls lf li, loopndx) =
  [cstms| for (int $id:loopndx = $ls; $id:loopndx < $lf; $id:loopndx += $li) {
            $stms:code
          }
        |]

loop :: [LB] -> ([Exp] -> [Stm]) -> [Stm]
loop [] f = [cstms| do {$stms:(f [])} while (0); |]
loop lbs f = foldl' loopstep (f args) (reverse $ zip lbs ndxes)
  where
    args = map mkVar ndxes
    mkVar vid = Var vid noLoc
    mkndx n = Id (printf "i__%d" n) noLoc
    ndxes = map mkndx [0 .. length lbs - 1]


looptest :: [Stm]
looptest = loop [lb 0 10 1, lb 0 20 2, lb 0 30 3] (\[i, j, k] -> [cstms| $i *= 10; $j *= 20; $k *= 30; |])
  where
    lb :: Int -> Int -> Int -> LB
    lb s f i = LB s f i

test :: Doc
test = ppr looptest

test0 :: Doc
test0 = ppr $ loop [] (\_ -> [cstms| return; |])
