{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators, 
             FlexibleContexts #-}

module Main where

import Control.Monad

import Flow

import Kernel.Binning
import Kernel.Data
import Kernel.FFT
import Kernel.Gridder
import Kernel.IO

-- ----------------------------------------------------------------------------
-- ---                             Functional                               ---
-- ----------------------------------------------------------------------------

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
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

gridderStrat :: Config -> Strategy ()
gridderStrat cfg = do

  -- Make point domain for visibilities
  tdom <- makeRangeDomain 0 (cfgPoints cfg)

  -- Create data flow for tag, bind it to FFT plans
  let gpar = cfgGrid cfg
      gcfpar = cfgGCF cfg

  -- Create data flow for visibilities, read in
  vis <- bindNew $ oskarReader tdom (cfgInput cfg) 0 0

  -- Data flows we want to calculate
  let gcfs = gcf vis
      gridded = grid vis gcfs createGrid
      result = gridder vis gcfs

  -- Create ranged domains for grid coordinates
  udoms <- makeRangeDomain 0 (gridWidth gpar)
  vdoms <- makeRangeDomain 0 (gridHeight gpar)
  let uvdoms = (udoms, vdoms)

  -- Split coordinate domain
  vdom <- split vdoms (gridTiles gpar)
  udom <- split udoms (gridTiles gpar)
  let uvdom = (udom, vdom)

  -- Create w-binned domain, split
  wdoms <- makeBinDomain $ binSizer gpar tdom uvdom vis
  wdom <- split wdoms (gridBins gpar)

  -- Load GCFs
  distribute wdom SeqSchedule $
    bind gcfs (gcfKernel gcfpar wdom)

  -- Bin visibilities (could distribute, but there's no benefit)
  rebind vis (binner gpar tdom uvdom wdom)

  -- Bind kernel rules
  bindRule createGrid (gridInit gcfpar uvdom)
  bindRule grid (gridKernel gpar gcfpar uvdoms wdom uvdom)

  -- Run gridding distributed
  distribute vdom ParSchedule $ distribute udom SeqSchedule $ do
    calculate gridded

  -- Compute the result by detiling & iFFT on result tiles
  bind createGrid (gridInitDetile uvdoms)
  bind gridded (gridDetiling gcfpar uvdom uvdoms gridded createGrid)
  bindRule idft (ifftKern gpar uvdoms)
  calculate result

  -- Write out
  void $ bindNew $ imageWriter gpar (cfgOutput cfg) result

main :: IO ()
main = do

  let gpar = GridPar { gridWidth = 2048
                     , gridHeight = 2048
                     , gridPitch = 2048
                     , gridTheta = 0.10
                     , gridFacets = 1
                     , gridTiles = 4
                     , gridBins = 10
                     }
      gcfpar = GCFPar { gcfSize = 16
                      , gcfOver = 8
                      , gcfFile = "gcf16.dat"
                      }
      config = Config
        { cfgInput  = "test_p00_s00_f00.vis"
        , cfgPoints = 32131 * 200
        , cfgLong   = 72.1 / 180 * pi -- probably wrong in some way
        , cfgLat    = 42.6 / 180 * pi -- ditto
        , cfgOutput = "out.img"
        , cfgGrid   = gpar
        , cfgGCF    = gcfpar
        }

  dumpSteps $ gridderStrat config
  execStrategyDNA $ gridderStrat config
