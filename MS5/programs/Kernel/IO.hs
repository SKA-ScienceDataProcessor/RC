
module Kernel.IO where

import Control.Applicative
import Control.Monad
import Foreign.C.Types ( CDouble(..) )
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Complex
import Data.Int ( Int32 )
import Data.IORef
import qualified Data.Map as Map

import OskarReader

import Flow.Builder
import Flow.Domain
import Flow.Vector
import Flow.Kernel
import Flow.Halide

import Kernel.Data

oskarReader :: Domain Range -> FilePath -> Int -> Int -> Kernel Vis
oskarReader dh file freq pol = rangeKernel0 "oskar reader" (rawVisRepr dh) $
 \domLow domHigh -> do

  taskData <- readOskarData file
  when (freq > tdChannels taskData) $
    fail "Attempted to read non-existent frequency channel from Oskar data!"

  -- Get data
  let baselinePoints = tdTimes taskData
      totalPoints = baselinePoints * tdBaselines taskData

  -- Allocate buffer for visibilities depending on region. Make sure
  -- that region is in range and aligned.
  when (domLow < 0 || domHigh > totalPoints) $
    fail $ "oskarReader: region out of bounds: " ++ show domLow ++ "-" ++ show domHigh ++
           " (only have " ++ show totalPoints ++ " points)"
  when ((domLow `mod` baselinePoints) /= 0) $
    fail $ "oskarReader: region not baseline-aligned: " ++ show domLow ++ "-" ++ show domHigh

  -- Go through baselines and collect our data into on big array
  let dblsPerPoint = 5
  visVector <- allocCVector $ dblsPerPoint * (domHigh - domLow)
  let bl0 = domLow `div` baselinePoints
      bl1 = (domHigh - 1) `div` baselinePoints
      CVector _ visp = visVector
  forM_ [bl0..bl1] $ \bl -> do
     forM_ [0..baselinePoints-1] $ \p -> do
       let off = (bl - bl0) * baselinePoints * dblsPerPoint + p * dblsPerPoint
           getUVW uvw = do CDouble d <- peek (tdUVWPtr taskData bl p uvw); return d
       pokeElemOff visp (off + 0) =<< getUVW 0
       pokeElemOff visp (off + 1) =<< getUVW 1
       pokeElemOff visp (off + 2) =<< getUVW 2
       v <- peek (tdVisibilityPtr taskData bl p freq pol)
       pokeElemOff visp (off + 3) (realPart v)
       pokeElemOff visp (off + 4) (imagPart v)

  -- Free all data, done
  finalizeTaskData taskData
  return visVector

-- | Dummy data representation for bin size vector
binSizeRepr :: VectorRepr Int32 ()
binSizeRepr = VectorRepr WriteAccess

-- | Kernel determining bin sizes. This is used to construct the bin
-- domain with enough data to allow us to calculate Halide buffer
-- sizes.
wBinSizer :: Domain Range -> Double -> Double -> Int -> Flow Vis -> Kernel ()
wBinSizer dh low high bins = mergingKernel "wBinSizer" (rawVisRepr dh :. Z) binSizeRepr $ \[(inVec,inds)] _ -> do

  -- Input size (range domain)
  let (_, inVis) :. (_, inWdt) :. Z  = halrDim (rawVisRepr dh) inds
      wfield = 2 -- u,v,w,r,i
      inVec' = castVector inVec :: Vector Double

  -- Make vector for bin sizes
  binVec <- allocCVector bins :: IO (Vector Int32)
  forM_ [0..bins-1] $ \i -> pokeVector binVec i 0
  forM_ [0..fromIntegral inVis-1] $ \i -> do
    w <- peekVector inVec' (i * fromIntegral inWdt + wfield)
    when (w >= low && w < high) $ do
      -- Note that strictly speaking this does not match the check
      -- below! Probably going to cause problems!
      let bin = floor ((w - low) / (high - low) * fromIntegral bins)
      pokeVector binVec bin =<< fmap (+1) (peekVector binVec bin)

  binVec' <- unmakeVector binVec 0 bins
  putStrLn $ "bin sizes = " ++ show binVec'

  return (castVector binVec)

-- | Kernel that extracts the visibilities for a bin from the
-- visibilities.
wBinner :: Domain Range -> Domain Bins -> Flow Vis -> Kernel Vis
wBinner dh bh = kernel "wBinner" (rawVisRepr dh :. Z) (visRepr bh) $ \[visPar] outds -> do

  -- Input size (range domain, assumed single region)
  let [(inds,inVec)] = Map.toList visPar
      (_, inVis) :. (_, inWdt) :. Z  = halrDim (rawVisRepr dh) inds
      wfield = 2 -- u,v,w,r,i
      inVec' = castVector inVec :: Vector Double
  when (inWdt /= 5) $ fail "wBinner: Unexpected data width!"

  -- Create bin vectors
  let binss = map (getBins . head) outds
      binLow  (low,_,_)  = low
      binHigh (_,high,_) = high
      binSize (_,_,size) = size
  (outVecs, outPtrs) <- unzip <$> forM binss (\bins -> do
    let size = sum (map binSize bins)
    vec@(CVector _ p) <- allocCVector (fromIntegral inWdt * size) :: IO (Vector Double)
    pRef <- newIORef p
    return (vec, map (\bin -> (binLow bin,pRef)) bins))
  let outPtrMap = Map.fromList $ concat outPtrs
      low = minimum $ map (minimum . map binHigh) binss
      high = maximum $ map (maximum . map binHigh) binss

  -- Bin visibilities
  forM_ [0..fromIntegral inVis-1] $ \i -> do

    -- Get w value, check range
    w <- peekVector inVec' (i * fromIntegral inWdt + wfield)
    when (w >= low && w < high) $ do

      -- Find bin this is supposed to go into, advance pointer
      let Just (_, pRef) = Map.lookupLE w outPtrMap
      p <- readIORef pRef
      writeIORef pRef (p `advancePtr` fromIntegral inWdt)

      -- Copy visibility
      let transfer f = poke (p `advancePtr` f) =<< peekVector inVec' (i * fromIntegral inWdt + f)
      transfer 0
      transfer 1
      transfer 2
      transfer 3
      transfer 4

  return (map castVector outVecs)

gcfKernel :: GCFPar -> Domain Bins -> Flow Tag -> Flow Vis -> Kernel GCFs
gcfKernel gcfp dh = mergingKernel "gcfs" (planRepr :. visRepr dh :. Z) (gcfsRepr dh gcfp) $ \_ doms -> do

  -- Simply read it from the file
  let size = nOfElements (halrDim (gcfsRepr dh gcfp) doms)
  v <- readCVector (gcfFile gcfp) size :: IO (Vector Double)
  return (castVector v)

imageWriter :: GridPar -> FilePath -> Flow Image -> Kernel ()
imageWriter gp = halideDump (imageRepr gp)

uvgWriter :: Domain Range -> Domain Range -> FilePath -> Flow UVGrid -> Kernel ()
uvgWriter ydom xdom = halideDump (uvgRepr ydom xdom)
