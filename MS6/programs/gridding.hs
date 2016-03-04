{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators, 
             FlexibleContexts #-}

module Main where

import Control.Monad

import Flow
import Flow.Kernel ( IsKernelDef )

import Kernel.Binning
import Kernel.Data
import Kernel.FFT
import Kernel.Gridder
import Kernel.IO
import Kernel.Scheduling

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
  (ddom, ixs) <- makeOskarDomain cfg 1
  let dkern :: IsKernelDef kf => kf -> kf
      dkern = regionKernel ddom
  tdom <- makeRangeDomain 0 (cfgPoints cfg)

  -- Create data flow for tag, bind it to FFT plans
  let gpar = cfgGrid cfg
      gcfpar = cfgGCF cfg

  -- Data flows we want to calculate
  vis <- uniq $ flow "vis" ixs
  let gcfs = gcf vis
      gridded = grid vis gcfs createGrid
      result = gridder vis gcfs

  -- Create ranged domains for grid coordinates
  udoms <- makeRangeDomain 0 (gridWidth gpar)
  vdoms <- makeRangeDomain 0 (gridHeight gpar)
  let uvdoms = (udoms, vdoms)

  -- Split and distribute over coordinate domain
  vdom <- split vdoms (gridTiles gpar)
  udom <- split udoms (gridTiles gpar)
  let uvdom = (udom, vdom)
  distribute vdom ParSchedule $ distribute udom ParSchedule $ do

    -- Read visibilities
    bind vis $ oskarReader ddom tdom (cfgInput cfg) 0 0 ixs

    -- Create w-binned domain, split
    wdoms <- makeBinDomain $ dkern $ binSizer gpar tdom uvdom vis
    wdom <- split wdoms (gridBins gpar)

    -- Load GCFs
    distribute wdom SeqSchedule $
      bind gcfs (dkern $ gcfKernel gcfpar wdom)

    -- Bin visibilities (could distribute, but there's no benefit)
    rebind vis (dkern $ binner gpar tdom uvdom wdom)

    -- Bind kernel rules
    bindRule createGrid (dkern $ gridInit gcfpar uvdom)
    bindRule grid $ dkern $ gridKernel GridKernelCPU gpar gcfpar uvdoms wdom uvdom

    -- Run gridding distributed
    calculate gridded

  -- Compute the result by detiling & iFFT on result tiles
  bind createGrid (dkern $ gridInitDetile uvdoms)
  bind gridded (dkern $ gridDetiling gcfpar uvdom uvdoms gridded createGrid)
  bindRule idft (dkern $ ifftKern gpar uvdoms)
  calculate result

  -- Write out
  void $ bindNew $ dkern $ imageWriter gpar (cfgOutput cfg) result

main :: IO ()
main = do

  let gpar = GridPar { gridWidth = 2048
                     , gridHeight = 2048
                     , gridPitch = 2048
                     , gridTheta = 0.10
                     , gridFacets = 1
                     , gridTiles = 2
                     , gridBins = 10
                     }
      gcfpar = GCFPar { gcfSize = 16
                      , gcfOver = 8
                      , gcfFile = "gcf16.dat"
                      }
      config = defaultConfig
        { cfgInput  = [OskarInput "test_p00_s00_f00.vis" 1 1]
        , cfgPoints = 32131 * 200
        , cfgLong   = 72.1 / 180 * pi -- probably wrong in some way
        , cfgLat    = 42.6 / 180 * pi -- ditto
        , cfgOutput = "out.img"
        , cfgGrid   = gpar
        , cfgGCF    = gcfpar
        , cfgNodes  = 1
        }

  dumpSteps $ gridderStrat config
  execStrategyDNA $ gridderStrat config
