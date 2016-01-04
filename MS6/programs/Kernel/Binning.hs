{-# LANGUAGE BangPatterns #-}

module Kernel.Binning ( binSizer, binner ) where

import Control.Arrow ( second )
import Control.Monad
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Int ( Int64 )
import Data.IORef
import qualified Data.Map as Map

import Flow.Builder
import Flow.Domain
import Flow.Vector
import Flow.Kernel
import Flow.Halide

import Kernel.Data

-- | Dummy data representation for bin size vector
type BinSizeRepr = RegionRepr Range (RegionRepr Range (VectorRepr Int64 ()))
binSizeRepr :: UVDom -> BinSizeRepr
binSizeRepr (udom, vdom) = RegionRepr udom $ RegionRepr vdom $ VectorRepr WriteAccess

-- | Field numbers in visibility data - we assume order u,v,w,r,i
ufield, vfield, wfield :: Int
[ufield, vfield, wfield] = [0..2]

-- | Kernel determining bin sizes. This is used to construct the bin
-- domain with enough data to allow us to calculate Halide buffer
-- sizes.
binSizer :: GridPar -> TDom -> UVDom -> Flow Vis -> Kernel ()
binSizer gpar tdom uvdom =
 kernel "binSizer" (rawVisRepr tdom :. Z) (binSizeRepr uvdom) $ \[visPar] rboxes -> do

  -- Input size (range domain)
  let [(inds,inVec)] = Map.toList visPar
      (_, inVis) :. (_, inWdt) :. Z  = halrDim (rawVisRepr tdom) inds
      inVec' = castVector inVec :: Vector Double

  -- Find range of coordinates
  let xy2uv = gridXY2UV gpar
      uvmin = xy2uv 0; uvmax = xy2uv (gridHeight gpar)
  (low, high0) <- (\f -> foldM f (0,0) [0..fromIntegral inVis-1]) $ \(low, high) i -> do
    u <- peekVector inVec' (i * fromIntegral inWdt + ufield)
    v <- peekVector inVec' (i * fromIntegral inWdt + vfield)
    w <- peekVector inVec' (i * fromIntegral inWdt + wfield)
    if u >= uvmin && u < uvmax && v >= uvmin && v < uvmax then do
      let !low' = min w low
          !high' = max w high
      return $! (low', high')
     else return (low, high)
  let high = high0 + (high0-low) * 0.0001

  -- Output sizes
  let bins = gridBins gpar
      binStart bin = low + fromIntegral bin * (high-low) / fromIntegral bins
  binVecs <- forM rboxes $ \[ureg, vreg] -> do
    -- Make a new vector
    binVec <- allocCVector (3 * bins) :: IO (Vector Int64)
    let binVecDbl = castVector binVec :: Vector Double
    forM_ [0..bins-1] $ \bin -> do
      -- Put start, end and count. This needs to be synchronised with
      -- what unpackBinDomain (Flow.Domain) expects!
      pokeVector binVecDbl (bin*3+0) (binStart bin)
      pokeVector binVecDbl (bin*3+1) (binStart (bin+1))
      pokeVector binVec    (bin*3+2) 0
    return (xy2uv $ fst $ regionRange ureg, (xy2uv $ fst $ regionRange vreg, binVec))
  let binVecMap = Map.fromListWith Map.union $
                  map (second (uncurry Map.singleton)) $
                  binVecs

  -- Make vector for bin sizes
  forM_ [0..fromIntegral inVis-1] $ \i -> do
    u <- peekVector inVec' (i * fromIntegral inWdt + ufield)
    v <- peekVector inVec' (i * fromIntegral inWdt + vfield)
    w <- peekVector inVec' (i * fromIntegral inWdt + wfield)
    when (u >= uvmin && u < uvmax && v >= uvmin && v < uvmax && w >= low && w <= high) $ do
      case Map.lookupLE u binVecMap >>= Map.lookupLE v . snd of
        Just (_, binVec) -> do
          let bin = floor ((w - low) / (high - low) * fromIntegral bins)
              bin' = max 0 $ min (bins-1) bin
          pokeVector binVec (3*bin'+2) =<< fmap (+1) (peekVector binVec (3*bin'+2))
        Nothing -> return ()

  {-
  forM_ binVecs $ \(u, (v, binVec)) -> do
    binVec' <- forM [0..bins-1] $ \bin -> peekVector binVec (bin*3+2)
    putStrLn $ "bin sizes " ++ show (u,v) ++ " = " ++ show binVec'
  -}

  return $ map (castVector . snd . snd) binVecs

-- | Kernel that splits up visibilities per u/v/w bins.
binner :: GridPar -> TDom -> UVDom -> WDom -> Flow Vis -> Kernel Vis
binner gpar tdom uvdom wdom =
 kernel "binner" (rawVisRepr tdom :. Z) (visRepr uvdom wdom) $ \[visPar] rboxes -> do

  -- Input size (range domain, assumed single region)
  let [(inds,inVec)] = Map.toList visPar
      (_, inVis) :. (_, inWdt) :. Z  = halrDim (rawVisRepr tdom) inds
      inVec' = castVector inVec :: Vector Double
  when (inWdt /= 5) $ fail "wBinner: Unexpected data width!"

  -- Create return vectors
  outVecs <- allocReturns allocCVector (visRepr uvdom wdom) rboxes

  -- Make pointer map
  let xy2uv (x,y) = (gridXY2UV gpar x, gridXY2UV gpar y)
  outPtrs <- forM outVecs $ \([ureg,vreg,wreg], CVector _ p) -> do
    pRef <- newIORef p
    return [ Map.singleton wl $ Map.singleton vl $ Map.singleton ul $
             ((ul,uh),(vl,vh),(wl,wh),pRef)
           | let (ul,uh) = xy2uv $ regionRange ureg
                 (vl,vh) = xy2uv $ regionRange vreg
           , (wl,wh,_) <- regionBins wreg
           ]
  let outPtrMap = Map.unionsWith (Map.unionWith Map.union) $ concat outPtrs

  -- Bin visibilities
  forM_ [0..fromIntegral inVis-1] $ \i -> do

    -- Get coordinates
    u <- peekVector inVec' (i * fromIntegral inWdt + ufield)
    v <- peekVector inVec' (i * fromIntegral inWdt + vfield)
    w <- peekVector inVec' (i * fromIntegral inWdt + wfield)

    -- Lookup and double-check range
    let lookupP x = fmap snd . Map.lookupLE x
    case lookupP u =<< lookupP v =<< lookupP w outPtrMap of
      Just ((ul,uh), (vl,vh), (wl,wh), pRef)
        | ul <= u && u < uh && vl <= v && v < vh && wl <= w && w < wh -> do

          -- Increase pointer
          p <- readIORef pRef
          writeIORef pRef (p `advancePtr` fromIntegral inWdt)

          -- Copy visibility
          let transfer f = poke (p `advancePtr` f) =<< peekVector inVec' (i * fromIntegral inWdt + f)
          transfer 0
          transfer 1
          transfer 2
          transfer 3
          transfer 4
      _otherwise -> return ()

  {-
  -- Check bin sizes
  forM_ outVecs $ \([ureg,vreg,wreg], CVector _ p) -> forM_ (regionBins wreg) $ \(w,_,s) -> do

    -- Get coordinates
    let u = fst $ xy2uv $ regionRange ureg
        v = fst $ xy2uv $ regionRange vreg

    putStr $ show ureg ++ " / " ++ show vreg ++ " / " ++ show wreg ++ ": "

    let lookupP x = fmap snd . Map.lookupLE x
    case lookupP u =<< lookupP v =<< lookupP w outPtrMap of
      Just ((ul,uh), (vl,vh), (wl,wh), pRef) -> do
          -- Check pointer
          p' <- readIORef pRef
          putStrLn $ show ((ul,uh), (vl,vh), (wl,wh)) ++ " -> " ++ show ((p' `minusPtr` p) `div` (5*8)) ++ " vs " ++ show s
      _otherwise -> putStrLn "???"
  -}

  return $ map (castVector . snd) outVecs
