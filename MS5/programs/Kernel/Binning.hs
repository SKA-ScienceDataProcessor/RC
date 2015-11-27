
module Kernel.Binning ( binSizer, binner ) where

import Control.Arrow ( second )
import Control.Monad
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Int ( Int32 )
import Data.IORef
import qualified Data.Map as Map

import Flow.Builder
import Flow.Domain
import Flow.Vector
import Flow.Kernel
import Flow.Halide

import Kernel.Data

-- | Dummy data representation for bin size vector
type BinSizeRepr = RegionRepr Range (RegionRepr Range (VectorRepr Int32 ()))
binSizeRepr :: Domain Range -> Domain Range -> BinSizeRepr
binSizeRepr udom vdom = RegionRepr udom $ RegionRepr vdom $ VectorRepr WriteAccess

-- | Field numbers in visibility data - we assume order u,v,w,r,i
ufield, vfield, wfield :: Int
[ufield, vfield, wfield] = [0..2]

-- | Kernel determining bin sizes. This is used to construct the bin
-- domain with enough data to allow us to calculate Halide buffer
-- sizes.
binSizer :: GridPar -> Domain Range -> UDom -> VDom -> Double -> Double -> Int -> Flow Vis -> Kernel ()
binSizer gpar dh udom vdom low high bins =
 kernel "binSizer" (rawVisRepr dh :. Z) (binSizeRepr udom vdom) $ \[visPar] rboxes -> do

  -- Input size (range domain)
  let [(inds,inVec)] = Map.toList visPar
      (_, inVis) :. (_, inWdt) :. Z  = halrDim (rawVisRepr dh) inds
      inVec' = castVector inVec :: Vector Double

  -- Output sizes
  let xy2uv xy = fromIntegral (xy - gridHeight gpar `div` 2) / gridTheta gpar
  binVecs <- forM rboxes $ \[ureg, vreg] -> do
    -- Make a new vector
    binVec <- allocCVector bins :: IO (Vector Int32)
    forM_ [0..bins-1] $ \i -> pokeVector binVec i 0
    return (xy2uv $ fst $ regionRange ureg, (xy2uv $ fst $ regionRange vreg, binVec))
  let binVecMap = Map.fromListWith Map.union $
                  map (second (uncurry Map.singleton)) $
                  binVecs

  -- Make vector for bin sizes
  forM_ [0..fromIntegral inVis-1] $ \i -> do
    u <- peekVector inVec' (i * fromIntegral inWdt + ufield)
    v <- peekVector inVec' (i * fromIntegral inWdt + vfield)
    w <- peekVector inVec' (i * fromIntegral inWdt + wfield)
    when (w >= low && w < high) $ do
      case Map.lookupLE u binVecMap >>= Map.lookupLE v . snd of
        Just (_, binVec) -> do
          let bin = floor ((w - low) / (high - low) * fromIntegral bins)
          pokeVector binVec bin =<< fmap (+1) (peekVector binVec bin)
        Nothing -> return ()

  {-
  forM_ binVecs $ \(u, (v, binVec)) -> do
    binVec' <- unmakeVector binVec 0 bins
    putStrLn $ "bin sizes " ++ show (u,v) ++ " = " ++ show binVec'
  -}

  return $ map (castVector . snd . snd) binVecs

-- | Kernel that splits up visibilities per u/v/w bins.
binner :: GridPar -> Domain Range -> Domain Range -> Domain Range -> Domain Bins -> Flow Vis -> Kernel Vis
binner gpar dh udom vdom wdom =
 kernel "binner" (rawVisRepr dh :. Z) (visRepr udom vdom wdom) $ \[visPar] rboxes -> do

  -- Input size (range domain, assumed single region)
  let [(inds,inVec)] = Map.toList visPar
      (_, inVis) :. (_, inWdt) :. Z  = halrDim (rawVisRepr dh) inds
      inVec' = castVector inVec :: Vector Double
  when (inWdt /= 5) $ fail "wBinner: Unexpected data width!"

  -- Create return vectors
  outVecs <- allocReturns allocCVector (visRepr udom vdom wdom) rboxes

  -- Make pointer map
  let xy2uv (x,y) = (c x, c y)
       where c z = fromIntegral (z - gridHeight gpar `div` 2) / gridTheta gpar
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
  forM_ outVecs $ \(rbox, v) -> do
    binVec' <- unmakeVector v 0 (vectorSize v)
    putStrLn $ "bin contents " ++ show rbox ++ " = (size " ++ show (length binVec') ++ ") = " ++ show (take 100 $ binVec')
  -}

  return $ map (castVector . snd) outVecs
