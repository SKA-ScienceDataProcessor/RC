{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators, StandaloneDeriving,
             FlexibleContexts #-}

module Main where

import Flow
import Flow.Vector

import Data.Typeable

-- ----------------------------------------------------------------------------
-- ---                             Functional                               ---
-- ----------------------------------------------------------------------------

-- Data tags
data Tag -- ^ Initialisation (e.g. FFT plans)
data Vis -- ^ Visibilities (File name to OSKAR / raw visibilities / binned ...)
data UVGrid -- ^ UV grid
data Image -- ^ Image
data GCFs -- ^ A set of GCFs

deriving instance Typeable Tag
deriving instance Typeable Vis
deriving instance Typeable UVGrid
deriving instance Typeable Image
deriving instance Typeable GCFs

-- Abstract kernel signatures.
createGrid :: Flow UVGrid
createGrid = flow "create grid"
grid :: Flow Vis -> Flow GCFs -> Flow UVGrid -> Flow UVGrid
grid = flow "grid"
idft :: Flow UVGrid -> Flow Image
idft = flow "idft"
gcf :: Flow Vis -> Flow GCFs
gcf = flow "gcf"

-- | Compound gridder actor
gridder :: Flow Vis -> Flow GCFs -> Flow Image
gridder vis gcfs = idft (grid vis gcfs createGrid)

-- ----------------------------------------------------------------------------
-- ---                               Kernels                                ---
-- ----------------------------------------------------------------------------

data Config = Config
  { cfgInput :: [FilePath]
  , cfgOutput :: FilePath
  , cfgGrid :: GridPar
  }
data GridPar = GridPar
  { gridWidth :: !Int  -- ^ Width of the uv-grid/image in pixels
  , gridHeight :: !Int -- ^ Neight of the uv-grid/image in pixels
  , gridPitch :: !Int  -- ^ Distance between rows in grid storage. Can
                       -- be larger than width if data is meant to be
                       -- padded.
  , gridTheta :: !Double  -- ^ Size of the field of view in radians
  }

type UVGridRepr = VectorRepr Double UVGrid
uvgRepr :: UVGridRepr
uvgRepr = VectorRepr WriteAccess

type ImageRepr = VectorRepr Double Image
imageRepr :: ImageRepr
imageRepr = VectorRepr WriteAccess

type PlanRepr = VectorRepr () Tag
planRepr :: VectorRepr () Tag
planRepr = VectorRepr ReadAccess

type RawVisRepr = VectorRepr () Vis
rawVisRepr :: RawVisRepr
rawVisRepr = VectorRepr ReadAccess

type VisRepr = VectorRepr () Vis
visRepr :: VisRepr
visRepr = VectorRepr ReadAccess

type GCFsRepr = VectorRepr () GCFs
gcfsRepr :: GCFsRepr
gcfsRepr = VectorRepr ReadAccess

oskarReader :: [FilePath] -> Kernel Vis
oskarReader _ = vecKernel0 "oskar reader" rawVisRepr $ do
  return nullVector
sorter :: Flow Vis -> Kernel Vis
sorter = vecKernel1 "sorter" rawVisRepr visRepr $ \_ -> do
  return nullVector

gcfKernel :: GridPar -> Flow Tag -> Flow Vis -> Kernel GCFs
gcfKernel _ = vecKernel2 "gcfs" planRepr visRepr gcfsRepr $ \_ _ -> do
  return nullVector

fftCreatePlans :: GridPar -> Kernel Tag
fftCreatePlans _ = vecKernel0 "fftPlans" planRepr $ do
  return nullVector
ifftKern :: GridPar -> Flow Tag -> Flow UVGrid -> Kernel Image
ifftKern _ = vecKernel2 "ifftKern" planRepr uvgRepr imageRepr $ \_ _ -> do
  return nullVector

gridInit :: GridPar -> Kernel UVGrid
gridInit _ = vecKernel0 "gridInit" uvgRepr $ do
  return nullVector
gridKernel :: GridPar -> Flow Vis -> Flow GCFs -> Flow UVGrid -> Kernel UVGrid
gridKernel _ = vecKernel3 "gridKernel" visRepr gcfsRepr uvgRepr uvgRepr $ \_ _ _ ->
  return nullVector

imageWriter :: FilePath -> Flow Image -> Kernel Image
imageWriter _ = vecKernel1 "image writer" imageRepr imageRepr $ \_ ->
  return nullVector

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

gridderStrat :: Config -> Strategy ()
gridderStrat cfg = do

  -- Make data set domain
  let dataSets = length (cfgInput cfg)
  dom <- makeRangeDomain 0 dataSets

  -- Create data flow for tag, bind it to FFT plans
  let gpar = cfgGrid cfg
  tag <- uniq (flow "tag")
  bind tag $ fftCreatePlans gpar

  -- Create data flow for visibilities, read in and sort
  let vis = flow "vis" tag
  bind vis $ oskarReader $ cfgInput cfg
  rebind vis sorter

  -- Compute the result
  let result = gridder vis (gcf vis)
  bindRule createGrid (gridInit gpar)
  bindRule grid (gridKernel gpar)
  bindRule idft (ifftKern gpar tag)
  bindRule gcf (gcfKernel gpar tag)
  calculate result

  -- Write out
  rebind result $ imageWriter (cfgOutput cfg)

main :: IO ()
main = do

  let gpar = GridPar { gridWidth = 1024
                     , gridHeight = 1024
                     , gridPitch = 1026
                     , gridTheta = 0.04
                     }
      config = Config
        { cfgInput  = [ "test_p00_s00_f00.vis", "test_p00_s00_f01.vis"
                      , "test_p00_s01_f00.vis", "test_p00_s01_f01.vis" ]
        , cfgOutput = "out.img"
        , cfgGrid   = gpar
        }

  dumpSteps $ gridderStrat config
  execStrategy $ gridderStrat config
