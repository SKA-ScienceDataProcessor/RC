{-# LANGUAGE
      QuasiQuotes
    , PackageImports
    , GADTs
    -- , DataKinds
    -- , PolyKinds
    -- , KindSignatures
    , TypeOperators
    , FlexibleInstances
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

-- To keep using ToExp instead of Int we should use geterogeneous lists.

data LB1 a b c where
  LB1 :: (ToExp a, ToExp b, ToExp c) => a -> b -> c -> LB1 a b c

infixr 5 :.

data Z = Z
data a :. b = a :. b

loopstep1 :: [Stm] -> Id -> LB1 a b c -> [Stm]
loopstep1 code loopndx (LB1 ls lf li) =
  [cstms| for (int $id:loopndx = $ls; $id:loopndx < $lf; $id:loopndx += $li) {
            $stms:code
          }
        |]

class LoopClass a where
  mkloop1 :: Int -> a -> [Stm] -> [Stm]
  iargs1 :: a -> [Exp] -- inverse
  len1 :: a -> Int

instance LoopClass Z where
  mkloop1 _ Z sms = sms
  iargs1 Z = []
  len1 Z = 0

mkId :: Int -> Id
mkId n = Id (printf "i__%d" n) noLoc

instance LoopClass zs => LoopClass (LB1 a b c :. zs) where
  mkloop1 n (x :. xs) sms =
    let ndx = mkId (n - len1 xs - 1) -- inverse
    in loopstep1 (mkloop1 n xs sms) ndx x
  iargs1 (_ :. xs) = Var (mkId $ len1 xs) noLoc : iargs1 xs
  len1 (_ :. xs) = 1 + len1 xs

loop1 :: LoopClass a => a -> ([Exp] -> [Stm]) -> [Stm]
loop1 lbs f
  | len1 lbs == 0 = [cstms| do {$stms:(f [])} while (0); |]
  | otherwise = mkloop1 (len1 lbs) lbs (f $ reverse $ iargs1 lbs)

looptest1 :: [Stm]
looptest1 = loop1 (lb 0 10 1 :. lb 0 20 2 :. lb 0 30 3 :. Z) (\[i, j, k] -> [cstms| $i *= 10; $j *= 20; $k *= 30; |])
  where
    lb :: Int -> Int -> Int -> LB1 Int Int Int
    lb s f i = LB1 s f i

test1 :: Doc
test1 = ppr looptest1

test01 :: Doc
test01 = ppr $ loop1 Z (\_ -> [cstms| return; |])
