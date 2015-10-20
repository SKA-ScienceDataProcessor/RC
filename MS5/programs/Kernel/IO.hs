
module Kernel.IO where

import Control.Applicative
import Control.Monad
import Foreign.Storable
import Foreign.C.Types ( CDouble(..) )
import Data.Complex

import OskarReader

import Flow.Builder
import Flow.Domain
import Flow.Vector
import Flow.Kernel
import Flow.Halide

import Kernel.Data

oskarReader :: DomainHandle Range -> FilePath -> Int -> Int -> Kernel Vis
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
  when (domLow < 0 || domHigh >= totalPoints) $
    fail $ "oskarReader: region out of bounds: " ++ show domLow ++ "-" ++ show domHigh
  when ((domLow `mod` baselinePoints) /= 0) $
    fail $ "oskarReader: region not baseline-aligned: " ++ show domLow ++ "-" ++ show domHigh
  visVector <- allocCVector (domHigh - domLow)

  -- Go through baselines and collect our data into on big array
  let dblsPerPoint = 5
      bl0 = domLow `div` baselinePoints `div` dblsPerPoint
      bl1 = (domHigh - 1) `div` baselinePoints `div` dblsPerPoint
      CVector _ visp = visVector
  forM_ [bl0..bl1] $ \bl -> do
     forM_ [0..baselinePoints-1] $ \p -> do
       let off = (bl - bl0) * baselinePoints + p * dblsPerPoint
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

sorter :: DomainHandle Range -> Flow Vis -> Kernel Vis
sorter dh = kernel "sorter" (rawVisRepr dh :. Z) (visRepr dh) $ \[(v,_)] _ ->

  -- TODO: We need to make a copy here to keep it from crashing. This
  -- will get resolved once we have implemented a way for kernels to
  -- declare that they consume their input.
  castVector <$> dupCVector (castVector v :: Vector Double)

gcfKernel :: GCFPar -> DomainHandle Range -> Flow Tag -> Flow Vis -> Kernel GCFs
gcfKernel gcfp dh = kernel "gcfs" (planRepr :. visRepr dh :. Z) (gcfsRepr gcfp) $ \_ doms -> do

  -- Simply read it from the file
  let size = nOfElements (halrDim (gcfsRepr gcfp) doms)
  v <- readCVector (gcfFile gcfp) size :: IO (Vector Double)
  return (castVector v)

imageWriter :: GridPar -> FilePath -> Flow Image -> Kernel Image
imageWriter gp file = kernel "image writer" (imageRepr gp :. Z) (imageRepr gp) $ \[(v,doms)] _ -> do

  let v' = castVector v :: Vector Double
      size = nOfElements (halrDim (imageRepr gp) doms)
  dumpVector' v' 0 size file
  return nullVector
