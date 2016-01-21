{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators,
             FlexibleContexts #-}

module Main where

import Control.Monad

import Flow
import Flow.Builder ( IsKernelDef )

import Kernel.Binning
import Kernel.Data
import Kernel.Facet
import Kernel.FFT
import Kernel.Gridder
import Kernel.IO
import Kernel.Scheduling

-- ----------------------------------------------------------------------------
-- ---                             Functional                               ---
-- ----------------------------------------------------------------------------

-- Gridding
createGrid :: Flow UVGrid
createGrid = flow "create grid"
grid :: Flow Vis -> Flow GCFs -> Flow UVGrid -> Flow UVGrid
grid = flow "grid"
gcf :: Flow Vis -> Flow GCFs
gcf = flow "gcf"

-- FFT
idft :: Flow UVGrid -> Flow Image
idft = flow "idft"

-- Image summation for continuum
createImage :: Flow Image
createImage = flow "create image"
sumImage :: Flow Image -> Flow Image -> Flow Image
sumImage = flow "sum image"

-- Compound actors
gridder :: Flow Vis -> Flow GCFs -> Flow Image
gridder vis gcfs = idft (grid vis gcfs createGrid)
summed :: Flow Vis -> Flow GCFs -> Flow Image
summed vis gcfs = sumImage (gridder vis gcfs) createImage

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

continuumStrat :: Config -> Strategy ()
continuumStrat cfg = do

  -- Make index and point domains for visibilities
  (ddoms, ixs, ddomSplit) <- makeOskarDomain cfg (cfgNodes cfg)
  tdom <- makeRangeDomain 0 (cfgPoints cfg)

  -- Split index domain - first into bins per node, then into
  -- individual data sets. The repeatSplit must be large enough to
  -- split the largest repeat possible.
  let repeatSplits = 1 + maximum (map oskarRepeat (cfgInput cfg))
  ddom <- split ddoms (cfgNodes cfg)
  ddom' <- split ddom repeatSplits
  let dkern :: IsKernelDef kf => kf -> kf
      dkern = regionKernel ddom'

  -- Create data flow for tag, bind it to FFT plans
  let gpar = cfgGrid cfg
      gcfpar = cfgGCF cfg

  -- Data flows we want to calculate
  vis <- uniq $ flow "vis" ixs
  let gcfs = gcf vis
      gridded = grid vis gcfs createGrid
      result = summed vis gcfs

  -- Create ranged domains for grid coordinates
  udoms <- makeRangeDomain 0 (gridWidth gpar)
  vdoms <- makeRangeDomain 0 (gridHeight gpar)
  let uvdoms = (udoms, vdoms)

  -- Split and distribute over coordinate domain
  vdom <- split vdoms (gridTiles gpar)
  udom <- split udoms (gridTiles gpar)
  let uvdom = (udom, vdom)

  -- Loop over nodes
  distribute ddom ParSchedule $ do

    -- Loop over data sets
    distribute ddom' SeqSchedule $ do

      -- Read visibilities
      rebind ixs $ ddomSplit ddoms ddom'
      bind vis $ oskarReader ddom' tdom (cfgInput cfg) 0 0 ixs

      -- Loop over tiles
      distribute vdom SeqSchedule $ distribute udom SeqSchedule $ do

        -- Create w-binned domain, split
        wdoms <- makeBinDomain $ dkern $ binSizer gpar tdom uvdom vis
        wdom <- split wdoms (gridBins gpar)

        -- Load GCFs
        distribute wdom SeqSchedule $
          bind gcfs (dkern $ gcfKernel gcfpar wdom)

        -- Bin visibilities (could distribute, but there's no benefit)
        rebind vis (dkern $ binner gpar tdom uvdom wdom)

        -- Bind kernel rules
        bindRule createGrid $ dkern $ gridInit gcfpar uvdom
        bindRule grid $ dkern $ gridKernel gpar gcfpar uvdoms wdom uvdom

        -- Run gridding distributed
        calculate gridded

      -- Compute the result by detiling & iFFT on tiles
      bind createGrid $ dkern $ gridInitDetile uvdoms
      bind gridded $ dkern $ gridDetiling gcfpar uvdom uvdoms gridded createGrid
      bindRule idft $ dkern $ ifftKern gpar uvdoms
      calculate $ gridder vis gcfs

    -- Sum up images locally
    bind createImage $ regionKernel ddom $ imageInit gpar
    bindRule sumImage $ imageSum gpar ddom' ddom
    calculate result

  bind createImage $ regionKernel ddoms $ imageInit gpar
  bind result $ imageSum gpar ddom ddoms result createImage

  -- Write out
  void $ bindNew $ regionKernel ddoms $
     imageWriter gpar (cfgOutput cfg) result

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
      config = Config
        { cfgInput  = [ OskarInput "test_p00_s00_f00.vis" 3 3
                      , OskarInput "test_p00_s00_f01.vis" 1 3
                      ]
        , cfgPoints = 32131 * 200
        , cfgLong   = 72.1 / 180 * pi -- probably wrong in some way
        , cfgLat    = 42.6 / 180 * pi -- ditto
        , cfgOutput = "out.img"
        , cfgGrid   = gpar
        , cfgGCF    = gcfpar
        , cfgNodes  = 4
        }

  dumpSteps $ continuumStrat config
  execStrategyDNA $ continuumStrat config
