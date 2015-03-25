{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
       TypeOperators
     , CPP
     , TypeFamilies
     , TypeSynonymInstances
     , FlexibleInstances
     , BangPatterns
     #-}

module Main where

import Data.List                             as P
import Prelude                               as P
import Text.Printf (printf)
import Data.Array.Accelerate                 as A
import Data.Array.Accelerate.Data.Complex    as A
import Data.Array.Accelerate.Math.FFT        as FFT
import Data.Array.Accelerate.Math.DFT.Centre as FFT
import Data.Array.Accelerate.IO              as A
-- import Data.Array.Accelerate.Interpreter     as A
import Data.Array.Accelerate.CUDA            as A

import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable(..))
import qualified Data.Vector.Storable        as VS
import qualified Data.ByteString.Unsafe      as BS
import qualified Data.ByteString             as BS

type CxDouble = Complex Double

mgrid :: Double -> Double -> Int -> [Double]
mgrid lo hi n =
  let
    diff = (hi - lo) / P.fromIntegral (n - 1)
    (half, r) = n `divMod` 2
    mid = if r > 0 then [(hi + lo) / 2.0] else []
  in P.take half (P.iterate (+ diff) lo) P.++ mid P.++ P.reverse (P.take half (P.iterate (\v -> v - diff) hi))

waf_full :: Int -> Int -> Exp Double -> Exp Double -> Acc (Array DIM2 CxDouble)
waf_full n over t2a wa = shift2D $ fft2D' Inverse no no (centre2D cpp0)
  where
    no = n * over
    Z :. nr :. nc = unlift (shape cp)
    cpp0 = A.generate (index2 (nr + padw + padw) (nc + padw + padw)) (pad cp)
    cp = A.map (\y -> exp $ A.constant ((0:+2) * (pi:+0) :: CxDouble) * (lift $ y :+ 0)) ph
    -- padding
    pad arr ix = cond (x <* 0 ||* x >=* w ||* y <* 0 ||* y >=* h)
                      (A.constant $ 0 :+ 0)
                      (arr ! index2 x y)
      where Z :. x0 :. y0 = unlift ix
            Z :. w :. h = unlift (shape arr)
            x = x0 - padw; y = y0 - padw
    padw = A.constant (n * (over - 1) `div` 2)
    --
    ph = A.map (\y -> wa * (1-sqrt(1-y))) r2
    --
    r2 = let
        (a, t) = ucsN
        l f x y = (f x, f y)
        (av, tv) = l (A.map (* t2a)) (use a) (use t)
        (av2, tv2) = l (A.map (^(2::Int))) av tv
      in A.zipWith (+) av2 tv2
    --
    ucsN = let
        grid = mgrid (-1.0) 1.0 n
        a = P.replicate n grid
        t = P.transpose a
        cvt arr = fromList (Z :. n :. n) (concat arr)
      in (cvt t, cvt a)



wextract :: Acc (Array DIM2 CxDouble) -> Exp Int -> Exp Int -> Exp Int -> Exp Int -> Acc (Array DIM2 CxDouble)
wextract arr i j over supp = exmid
  where
    exmid = backpermute (lift $ Z :. diam :. diam) mapex xnorm
    diam = 2 * supp + 1
    mapex ndx =
      let i2 = unindex2 ndx
      in index2 (A.fst i2 + outnr `div` 2 - supp) (A.snd i2 + outnc `div` 2 - supp)
    xnorm = A.map (* invsx) x
    invsx = lift (1.0 / sx :+ 0.0)
    sx = the (A.sum $ A.map real x')
    x = backpermute outshape mapndx arr
    x' = backpermute outshape mapndx arr -- duplicated to prevent sharing
    outshape = lift $ Z :. outnr :. outnc
    outnr = (nr - i) `div` over
    outnc = (nc - j) `div` over
    mapndx ndx =
      let i2 = unindex2 ndx
      in index2 (A.fst i2 * over + i) (A.snd i2 * over + j)
    Z :. nr :. nc = unlift (shape arr) :: Z :. Exp Int :. Exp Int

wkernaf_conj :: Exp Int -> Exp Int -> Exp Int -> Exp Int -> Acc (Array DIM2 CxDouble) -> Acc (Array DIM2 CxDouble)
wkernaf_conj supp over overx overy arr =
  A.map conjugate $ wextract arr overx overy over supp

-- Quick and dirty storable for Complex
instance Storable CxDouble where
  sizeOf _ = 16
  alignment _ = 16
  peek p = do
    re <- peek (castPtr p)
    im <- peekByteOff p 8
    return (re :+ im)
  poke p (re :+ im) = do
    poke (castPtr p) re
    pokeByteOff p 8 im

-- Why on earth accelerate has complex computations,
-- but no facilities to export complex vectors (only pair of double vectors)???
toFlatVector :: Shape sh => Array sh CxDouble -> VS.Vector CxDouble
toFlatVector a =
  let ((_, res), ims) = toVectors a
  in VS.zipWith (:+) res ims

mkScalar :: Elt e => e -> Scalar e
mkScalar = fromFunction Z . const

-- these are calculated for the last config
main :: IO ()
main = do

    let over = 8
        waf_full' w = waf_full 256 over 0.024873 (the w)
        over_layers = P.map (run1 waf_full' . mkScalar) $
                      P.take 32 $ P.iterate (+61.884644) 0.0
    --print waf_full'

    let wkernaf_conj' pars = wkernaf_conj (A.constant 16) (A.constant over) (the ia) (the ja) wa
            where (ia, ja, wa) = unlift pars
    --print wkernaf_conj'

    flip mapM_ (P.zip over_layers [0::Int ..]) $ \(ol, n) -> do
         let gcf_layer = VS.concat $ P.map toFlatVector
                         [ run1 wkernaf_conj' (mkScalar i, mkScalar j, ol)
                           | i <- [0..7], j <- [0..7] ]

         let cxdsize = sizeOf (undefined :: CxDouble)
             (fptr, len) = VS.unsafeToForeignPtr0 gcf_layer
             file = printf "GCF%02d.dat" n
         withForeignPtr fptr $ \p ->
             BS.unsafePackCStringLen (castPtr p, len * cxdsize) >>= BS.writeFile file
