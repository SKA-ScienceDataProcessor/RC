{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators, 
             FlexibleContexts #-}

module Main where

import Flow

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
  dom <- makeRangeDomain 0 (cfgPoints cfg)

  -- Create data flow for tag, bind it to FFT plans
  let gpar = cfgGrid cfg
      gcfpar = cfgGCF cfg
  tag <- uniq (flow "tag")
  bind tag $ fftCreatePlans gpar

  -- Create data flow for visibilities, read in and sort
  let vis = flow "vis" tag
  bind vis $ oskarReader dom (cfgInput cfg) 0 0
  rebind vis $ sorter dom

  -- Compute the result
  let result = gridder vis (gcf vis)
  bindRule gcf (gcfKernel gcfpar dom tag)
  bindRule createGrid (gridInit gpar)
  bindRule grid (gridKernel gpar gcfpar dom)
  bindRule idft (ifftKern gpar tag)
  calculate result

  -- Write out
  rebind result $ imageWriter gpar (cfgOutput cfg)

main :: IO ()
main = do

  let gpar = GridPar { gridWidth = 1024
                     , gridHeight = 1024
                     , gridPitch = 1026
                     , gridTheta = 0.04
                     }
      gcfpar = GCFPar { gcfSize = 16
                      , gcfOver = 8
                      , gcfFile = "gcf0.dat"
                      }
      config = Config
        { cfgInput  = "test_p00_s00_f00.vis"
        , cfgPoints = 32131 * 200
        , cfgOutput = "out.img"
        , cfgGrid   = gpar
        , cfgGCF    = gcfpar
        }

  dumpSteps $ gridderStrat config
  execStrategy $ gridderStrat config
