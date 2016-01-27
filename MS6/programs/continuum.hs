{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators,
             FlexibleContexts #-}

module Main where

import Control.Monad

import Flow
import Flow.Builder ( IsKernelDef )

import Kernel.Binning
import Kernel.Cleaning
import Kernel.Data
import Kernel.Facet
import Kernel.FFT
import Kernel.Gridder
import Kernel.IO
import Kernel.Scheduling

import System.Environment

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

-- Cleaning
psfVis :: Flow Vis -> Flow Vis
psfVis = flow "make PSF visibilities"
clean :: Flow Image -> Flow Image -> Flow Cleaned
clean = flow "clean"
splitModel :: Flow Cleaned -> Flow Image
splitModel = flow "model from cleaning"
splitResidual :: Flow Cleaned -> Flow Image
splitResidual = flow "residual from cleaning"

-- Compound actors
gridder :: Flow Vis -> Flow GCFs -> Flow Image
gridder vis gcfs = idft (grid vis gcfs createGrid)
summed :: Flow Vis -> Flow GCFs -> Flow Image
summed vis gcfs = sumImage (gridder vis gcfs) createImage

psf :: Flow Vis -> Flow GCFs -> Flow Image
psf vis gcfs = summed (psfVis vis) gcfs
model :: Flow Vis -> Flow GCFs -> Flow Image
model vis gcfs = splitModel $ clean (psf vis gcfs) (summed vis gcfs)
residual :: Flow Vis -> Flow GCFs -> Flow Image
residual vis gcfs = splitResidual $ clean (psf vis gcfs) (summed vis gcfs)

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

-- | Implement continuum gridding (@summed vis gcfs@) for the given
-- input 'Flow's over a number of datasets given by the data set
-- domains @ddoms@. Internally, we will distribute twice over @DDom@
-- and once over @UVDom@.
continuumGridStrat :: Config -> [DDom] -> TDom -> [UVDom]
                   -> Flow Index -> Flow Vis -> Flow Vis -> Flow GCFs
                   -> Strategy (Flow Image)
continuumGridStrat cfg [ddomss,ddoms,ddom] tdom [uvdoms,uvdom] ixs rvis vis gcfs =
 implementing (summed vis gcfs) $ do

  -- Helpers
  let dkern :: IsKernelDef kf => kf -> kf
      dkern = regionKernel ddom
      gpar = cfgGrid cfg
      gcfpar = cfgGCF cfg

  -- Intermediate flow nodes
  let gridded = grid vis gcfs createGrid
      result = summed vis gcfs

  -- Distribute over nodes
  distribute ddoms ParSchedule $ do

    -- Loop over data sets
    distribute ddom SeqSchedule $ do

      -- Read visibilities
      rebind ixs $ scheduleSplit ddomss ddom
      bind rvis $ oskarReader ddom tdom (cfgInput cfg) 0 0 ixs

      -- Loop over tiles
      distribute (snd uvdom) SeqSchedule $ distribute (fst uvdom) SeqSchedule $ do

        -- Create w-binned domain, split
        wdoms <- makeBinDomain $ dkern $ binSizer gpar tdom uvdom vis
        wdom <- split wdoms (gridBins gpar)

        -- Load GCFs
        distribute wdom SeqSchedule $
          bind gcfs (dkern $ gcfKernel gcfpar wdom)

        -- Bin visibilities (could distribute, but there's no benefit)
        rebind vis (dkern $ binner gpar tdom uvdom wdom)

        -- Bind kernel rules
        bind createGrid $ dkern $ gridInit gcfpar uvdom
        bindRule grid $ dkern $ gridKernel gpar gcfpar uvdoms wdom uvdom

        -- Run gridding distributed
        calculate gridded

      -- Compute the result by detiling & iFFT on tiles
      bind createGrid $ dkern $ gridInitDetile uvdoms
      bind gridded $ dkern $ gridDetiling gcfpar uvdom uvdoms gridded createGrid
      bindRule idft $ dkern $ ifftKern gpar uvdoms
      calculate $ gridder vis gcfs

    -- Sum up images locally
    bind createImage $ regionKernel ddoms $ imageInit gpar
    bindRule sumImage $ imageSum gpar ddom ddoms
    calculate result

  recover result $ regionKernel ddoms $ imageInit gpar
  bind createImage $ regionKernel ddomss $ imageInit gpar
  bind result $ imageSum gpar ddoms ddomss result createImage

continuumGridStrat _ _ _ _ _ _ _ _ = fail "continuumGridStrat: Not enough domain splits provided!"

continuumStrat :: Config -> Strategy ()
continuumStrat cfg = do

  -- Make index and point domains for visibilities
  (ddomss, ixs) <- makeOskarDomain cfg (cfgNodes cfg)
  tdom <- makeRangeDomain 0 (cfgPoints cfg)

  -- Split index domain - first into bins per node, then into
  -- individual data sets. The repeatSplit must be large enough to
  -- split the largest repeat possible.
  let repeatSplits = 1 + maximum (map oskarRepeat (cfgInput cfg))
  ddoms <- split ddomss (cfgNodes cfg)
  ddom <- split ddoms repeatSplits

  -- Data flows we want to calculate
  vis <- uniq $ flow "vis" ixs
  let gcfs = gcf vis
      pvis = psfVis vis

  -- Create ranged domains for grid coordinates
  let gpar = cfgGrid cfg
  udoms <- makeRangeDomain 0 (gridWidth gpar)
  vdoms <- makeRangeDomain 0 (gridHeight gpar)
  let uvdoms = (udoms, vdoms)

  -- Split and distribute over coordinate domain
  vdom <- split vdoms (gridTiles gpar)
  udom <- split udoms (gridTiles gpar)
  let uvdom = (udom, vdom)

  -- Compute PSF
  bindRule psfVis $ regionKernel ddom $ psfVisKernel tdom
  let gridStrat = continuumGridStrat cfg [ddomss, ddoms, ddom] tdom [uvdoms, uvdom] ixs
  psfFlow <- gridStrat vis pvis gcfs
  void $ bindNew $ regionKernel ddomss $ imageWriter gpar "psf.img" psfFlow

  -- Gridding
  sumFlow <- gridStrat vis vis gcfs
  void $ bindNew $ regionKernel ddomss $ imageWriter gpar "gridded.img" sumFlow

  -- Clean
  let cpar = cfgClean cfg
  bindRule (\psfImg -> splitModel . clean psfImg) $
    regionKernel ddomss $ cleanModel gpar cpar
  bindRule (\psfImg -> splitResidual . clean psfImg) $
    regionKernel ddomss $ cleanResidual gpar cpar

  -- Write out residual & model
  void $ bindNew $ regionKernel ddomss $
     imageWriter gpar (cfgOutput cfg) $ residual vis gcfs
  void $ bindNew $ regionKernel ddomss $
     imageWriter gpar (cfgOutput cfg ++ ".mod") $ model vis gcfs

main :: IO ()
main = do

  let gpar = GridPar { gridWidth = 2048
                     , gridHeight = 2048
                     , gridPitch = 2048
                     , gridTheta = 0.10
                     , gridFacets = 1
                     , gridTiles = 1
                     , gridBins = 1
                     }
      gcfpar = GCFPar { gcfSize = 16
                      , gcfOver = 8
                      , gcfFile = "gcf16.dat"
                      }
      cleanPar = CleanPar
        { cleanGain      = 0.9
        , cleanThreshold = 1e5
        , cleanCycles    = 10
        }
      config = defaultConfig
        { cfgInput  = [ OskarInput "test_p00_s00_f00.vis" 2 2
                      , OskarInput "test_p00_s00_f01.vis" 2 2
                      ]
        , cfgPoints = 32131 * 200
        , cfgNodes  = 4
        , cfgOutput = "out.img"
        , cfgGrid   = gpar
        , cfgGCF    = gcfpar
        , cfgClean  = cleanPar
        }

  -- Show strategy - but only for the root process
  args <- getArgs
  when (not ("--internal-rank" `elem` args)) $ do
    dumpSteps $ continuumStrat config
    putStrLn "----------------------------------------------------------------"
    putStrLn ""
  -- Execute strategy
  execStrategyDNA $ continuumStrat config
