{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators,
             FlexibleContexts #-}

module Main where

import Control.Monad

import Flow
import Flow.Kernel ( IsKernelDef )

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

-- Abstract kernel signatures.
createGrid :: Flow UVGrid
createGrid = flow "create grid"
grid :: Flow Vis -> Flow GCFs -> Flow UVGrid -> Flow UVGrid
grid = flow "grid"
idft :: Flow UVGrid -> Flow Image
idft = flow "idft"
gcf :: Flow Vis -> Flow GCFs
gcf = flow "gcf"

createImage :: Flow Image
createImage = flow "create image"
facetSum :: Flow Image -> Flow Image -> Flow Image
facetSum = flow "facet sum"

-- | Compound gridder actor
gridder :: Flow Vis -> Flow GCFs -> Flow Image
gridder vis gcfs = idft (grid vis gcfs createGrid)

-- | Facetted compound gridder actor
facetGridder :: Flow Vis -> Flow GCFs -> Flow Image
facetGridder vis gcfs = facetSum (gridder vis gcfs) createImage

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

gridderStrat :: Config -> Strategy ()
gridderStrat cfg = do
  let gpar = cfgGrid cfg
      gcfpar = cfgGCF cfg

  -- Make index and point domains for visibilities
  (ddom, ixs) <- makeOskarDomain cfg 1
  let dkern :: IsKernelDef kf => kf -> kf
      dkern = regionKernel ddom
  tdom <- makeRangeDomain 0 (cfgPoints cfg)

  -- Create ranged domains for image coordinates
  ldoms <- makeRangeDomain 0 (gridImageWidth gpar)
  mdoms <- makeRangeDomain 0 (gridImageHeight gpar)
  ldom <- split ldoms (gridFacets gpar)
  mdom <- split mdoms (gridFacets gpar)
  let lmdom = (ldom, mdom)

  -- Create ranged domains for grid coordinates
  udoms <- makeRangeDomain 0 (gridWidth gpar)
  vdoms <- makeRangeDomain 0 (gridHeight gpar)
  udom <- split udoms (gridTiles gpar)
  vdom <- split vdoms (gridTiles gpar)
  let uvdoms = (udoms, vdoms); uvdom = (udom, vdom)

  -- Data flows we want to calculate
  vis <- uniq $ flow "vis" ixs
  let gcfs = gcf vis
      gridded = grid vis gcfs createGrid
      facets = gridder vis gcfs
      result = facetGridder vis gcfs

  distribute ldom ParSchedule $ distribute mdom ParSchedule $ do
    let rkern :: IsKernelDef kf => kf -> kf
        rkern = dkern . regionKernel ldom . regionKernel mdom

    -- Read visibilities in
    bind vis $ oskarReader ddom tdom (cfgInput cfg) 0 0 ixs

    -- Rotate visibilities
    rebind vis (dkern $ rotateKernel cfg lmdom tdom)

    -- Create w-binned domain, split
    wdoms <- makeBinDomain (rkern $ binSizer gpar tdom uvdom vis)
    wdom <- split wdoms (gridBins gpar)

    -- Load GCFs
    distribute wdom SeqSchedule $
      bind gcfs (rkern $ gcfKernel gcfpar wdom)

    -- Bin visibilities (could distribute, but there's no benefit)
    rebind vis (rkern $ binner gpar tdom uvdom wdom)

    -- Bind kernel rules
    bindRule createGrid (rkern $ gridInit gcfpar uvdom)
    bindRule grid (rkern $ hints [floatHint] $ gridKernel gpar gcfpar uvdoms wdom uvdom)

    -- Run gridding distributed
    distribute vdom ParSchedule $ distribute udom ParSchedule $ do
      calculate gridded

    -- Compute the result by detiling & iFFT on result tiles
    bind createGrid (rkern $ gridInitDetile uvdoms)
    bind gridded (rkern $ gridDetiling gcfpar uvdom uvdoms gridded createGrid)
    bindRule idft (rkern $ hints [floatHint] $ ifftKern gpar uvdoms)
    calculate facets

  -- Write out
  bindRule createImage $ dkern $ imageInit gpar
  bind result $ dkern $ imageDefacet gpar lmdom facets createImage
  void $ bindNew $ dkern $ imageWriter gpar (cfgOutput cfg) result

main :: IO ()
main = do

  let gpar = GridPar { gridWidth  = 2048
                     , gridHeight = 2048
                     , gridPitch  = 2048
                     , gridTheta  = 0.10
                     , gridFacets = 3
                     , gridTiles  = 1
                     , gridBins   = 10
                     }
      gcfpar = GCFPar { gcfSize = 16
                      , gcfOver = 8
                      , gcfFile = "gcf0.dat"
                      }
      config = Config
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
