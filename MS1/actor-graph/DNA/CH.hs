{-# LANGUAGE GADTs #-}
-- | Library functions for the 
module DNA.CH where


import DNA.AST

import qualified Data.Vector.Storable as S

zipArray :: (S.Storable a, S.Storable b, S.Storable c, Eq sh)
         => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipArray f (Array shA va) (Array shB vb)
  | shA /= shB = error "Bad vector shape"
  | otherwise  = Array shA (S.zipWith f va vb)

foldArray :: (S.Storable a)
          => (a -> a -> a) -> a -> (Array sh a) -> a
foldArray f x0 (Array _ v) = S.foldl' f x0 v

generateArray :: (IsShape sh, S.Storable a)
              => (Int -> a) -> sh -> Array sh a
generateArray f sh =
  case reifyShape sh of
    ShShape -> case sh of
                 Shape n -> Array sh (S.generate n f)
    ShSlice -> case sh of
                 Slice off n -> Array sh (S.generate n (\i -> f (i + off)))
    
