
module Oskar where

import Control.Monad
import DNA.Channel.File
import Foreign.Ptr
import Foreign.Storable

import OskarReader

import Config
import Data
import Vector

-- | Just reads the header from an Oskar file, to determine how much
-- different data sets they contain.
--
-- TODO: Currently still reads the whole file!
readOskarHeader :: FileChan OskarData -> String -> IO ([Polar], [Int])
readOskarHeader chan file = do
  taskData <- readOskarData (getFileChan chan file)
  finalizeTaskData taskData
  return ([XX,YY,XY,YX], [0..tdChannels taskData-1])

-- | Read visibilities from an Oskar file.
--
-- NOTE: At this point we read only one polarisation. This is with
-- high probability not what we want in the long run, and might
-- already be wrong for MS4, we'll have to see.
readOskar :: FileChan OskarData -> String -> Int -> Polar -> IO Vis
readOskar chan file freq polar = do
  taskData <- readOskarData (getFileChan chan file)
  when (freq > tdChannels taskData) $
      fail "Attempted to read non-existent frequency channel from Oskar data!"

  -- Positions are already in the format we want, but we somewhat
  -- awkwardly have to re-rearrange the visibilities here.
  let baselinePoints = tdTimes taskData
      totalPoints = baselinePoints * tdBaselines taskData
      pos = CVector totalPoints $ castPtr $ tdUVWs taskData
  vis <- allocCVector (tdBaselines taskData * baselinePoints)
  let CVector _ visp = vis

  -- Go through baselines and collect our data
  let oskarPolar = 4 -- hardcoded
      oskarBaselinePoints = tdTimes taskData * tdChannels taskData * oskarPolar
  baselines <- forM [0..tdBaselines taskData-1] $ \bl -> do
     forM_ [0..baselinePoints-1] $ \p -> do
         pokeElemOff visp (bl * baselinePoints + p) =<< peek (tdVisibilityPtr taskData bl p freq (fromEnum polar))

     -- Generate baseline structure
     wmaxmin <- peekElemOff (tdBlMaxMin taskData) bl
     return VisBaseline { vblOffset = bl * baselinePoints
                        , vblPoints = tdTimes taskData
                        , vblMinW = mmMinW wmaxmin
                        , vblMaxW = mmMaxW wmaxmin
                        }

  -- Free all data except positions (which we continue using)
  finalizeTaskData taskData{tdUVWs = nullPtr}

  return Vis { visMinW = mtxMinW $ tdMetrix taskData
             , visMaxW = mtxMaxW $ tdMetrix taskData
             , visTimesteps = tdTimes taskData
             , visBaselines = baselines
             , visPositions = pos
             , visData = vis
             , visBinData = nullVector
             }
