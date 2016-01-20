{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators,
             FlexibleContexts #-}

module Main where

import Control.Monad

import Flow
import Flow.Builder ( IsKernelDef )

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
imageSum :: Flow Image -> Flow Image
imageSum = flow "image sum"

-- | Compound gridder actor
gridder :: Flow Vis -> Flow GCFs -> Flow Image
gridder vis gcfs = idft (grid vis gcfs createGrid)

summed :: Flow Vis -> Flow GCFs -> Flow Image
summed vis gcfs = imageSum $ gridder vis gcfs

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

continuumStrat :: Config -> Strategy ()
continuumStrat cfg = do

  -- Make index and point domains for visibilities
  (ddoms, ixs, ddomSplit) <- makeOskarDomain cfg (cfgNodes cfg)
  tdom <- makeRangeDomain 0 (cfgPoints cfg)

  -- Split index domain - first into bins per node, then into individual data sets
  ddom <- split ddoms (cfgNodes cfg)
  ddom' <- split ddom (maximum (map oskarRepeat (cfgInput cfg)))
  let dkern :: IsKernelDef kf => kf -> kf
      dkern = regionKernel ddom'

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

  -- Loop over nodes
  distribute ddom ParSchedule $ do
    distribute ddom' SeqSchedule $ do

      -- Loop over tiles
      distribute vdom SeqSchedule $ distribute udom SeqSchedule $ do

        -- Read visibilities
        rebind ixs $ ddomSplit ddoms ddom'
        bind vis $ oskarReader ddom' tdom (cfgInput cfg) 0 0 ixs

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

      -- Compute the result by detiling & iFFT on result tiles
      bind createGrid $ dkern $ gridInitDetile uvdoms
      bind gridded $ dkern $ gridDetiling gcfpar uvdom uvdoms gridded createGrid
      bindRule idft $ dkern $ ifftKern gpar uvdoms
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
