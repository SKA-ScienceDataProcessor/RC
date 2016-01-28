{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators,
             FlexibleContexts #-}

module Main where

import Control.Monad

import Flow
import Flow.Builder ( IsKernelDef )

import Kernel.Binning
import Kernel.Cleaning
import Kernel.Data
import Kernel.Degrid
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
degrid :: Flow GCFs -> Flow UVGrid -> Flow Vis -> Flow Vis
degrid = flow "degrid"
gcf :: Flow Vis -> Flow GCFs
gcf = flow "gcf"

-- FFT
dft :: Flow Image -> Flow UVGrid
dft = flow "dft"
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
clean :: Flow Image -> Flow Image -> Flow Image -> Flow Cleaned
clean = flow "clean"
splitModel :: Flow Cleaned -> Flow Image
splitModel = flow "model from cleaning"
splitResidual :: Flow Cleaned -> Flow Image
splitResidual = flow "residual from cleaning"

-- Compound actors
gridder :: Flow Vis -> Flow Vis -> Flow Image
gridder vis0 vis = idft (grid vis (gcf vis0) createGrid)
summed :: Flow Vis -> Flow Vis -> Flow Image
summed vis0 vis = sumImage (gridder vis0 vis) createImage

-- | Calculate a point spread function, which tells us the image shape
-- of a single point source in the middle of the field of view
psf :: Flow Vis -> Flow Image
psf vis = summed vis (psfVis vis)

-- | Update a model by gridding the given (possibly corrected)
-- visibilities and cleaning the result.
model :: Flow Vis -> Flow Vis -> Flow Image -> Flow Image
model vis0 vis mod = splitModel $ clean (psf vis0) (summed vis0 vis) mod

-- | Calculate a residual by gridding the given visibilities and
-- cleaning the result.
residual :: Flow Vis -> Flow Vis -> Flow Image
residual vis0 vis = splitResidual $ clean (psf vis0) (summed vis0 vis) createImage

-- | Degrid a model, producing corrected visibilities where we
-- have attempted to eliminate the effects of sources in the model.
degridModel :: Flow Vis -> Flow Image -> Flow Vis
degridModel vis mod = degrid (gcf vis) (dft mod) vis

-- | Major loop iteration: From visibilities infer components in the
-- image and return the updated model.
loopIter :: Flow Vis -> Flow Image -> Flow Image
loopIter vis mod = model vis (degridModel vis mod) mod
-- | Final major loop iteration: Do the same steps as usual, but
-- return just the residual.
finalLoopIter :: Flow Vis -> Flow Image -> Flow Image
finalLoopIter vis mod = residual vis (degridModel vis mod)

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

-- | Implement one major iteration of continuum gridding (@loopIter@) for
-- the given input 'Flow's over a number of datasets given by the data
-- set domains @ddoms@. Internally, we will distribute twice over
-- @DDom@ and once over @UVDom@.
continuumGridStrat :: Config -> [DDom] -> TDom -> [UVDom]
                   -> Flow Index -> Flow Vis -> Flow Vis
                   -> Strategy (Flow Image)
continuumGridStrat cfg [ddomss,ddoms,ddom] tdom [uvdoms,uvdom] ixs vis0 vis
 = implementing (summed vis0 vis) $ do

  -- Helpers
  let dkern :: IsKernelDef kf => kf -> kf
      dkern = regionKernel ddom
      gpar = cfgGrid cfg
      gcfpar = cfgGCF cfg

  -- Intermediate Flow nodes
  let gridded = grid vis (gcf vis0) createGrid -- grid from vis
      summed' = summed vis0 vis  -- images, summed over channels

  -- Distribute over nodes
  distribute ddoms ParSchedule $ do

    -- Loop over data sets
    distribute ddom SeqSchedule $ do

      -- Read in visibilities
      rebind ixs $ scheduleSplit ddomss ddom
      bind vis0 $ oskarReader ddom tdom (cfgInput cfg) 0 0 ixs

      -- Create w-binned domain, split
      wdoms <- makeBinDomain $ dkern $ binSizer gpar tdom uvdom vis0
      wdom <- split wdoms (gridBins gpar)

      -- Loop over tiles
      distribute (snd uvdom) SeqSchedule $ distribute (fst uvdom) SeqSchedule $ do

        -- Load GCFs
        bindRule gcf $ dkern $ const $ gcfKernel gcfpar wdom
        distribute wdom SeqSchedule $ calculate $ gcf vis0

        -- Bin visibilities (could distribute, but there's no benefit)
        rebind vis0 $ dkern $ binner gpar tdom uvdom wdom

        -- Degrid / generate PSF (depending on vis)
        bindRule degrid $ degridKernel gpar gcfpar ddomss ddom uvdom wdom uvdoms
        bindRule psfVis $ dkern $ psfVisKernel uvdom wdom
        calculate vis

        -- Gridding
        bind createGrid $ dkern $ gridInit gcfpar uvdom
        bindRule grid $ dkern $ gridKernel gpar gcfpar uvdoms wdom uvdom
        calculate gridded

      -- Compute the result by detiling & iFFT on tiles
      bind createGrid $ dkern $ gridInitDetile uvdoms
      bind gridded $ dkern $ gridDetiling gcfpar uvdom uvdoms gridded createGrid
      bindRule idft $ dkern $ ifftKern gpar uvdoms
      calculate $ idft gridded

    -- Sum up images locally
    bind createImage $ regionKernel ddoms $ imageInit gpar
    bindRule sumImage $ imageSum gpar ddom ddoms
    calculate summed'

  -- Sum up images over nodes
  recover summed' $ regionKernel ddoms $ imageInit gpar
  bind createImage $ regionKernel ddomss $ imageInit gpar
  bind summed' $ imageSum gpar ddoms ddomss summed' createImage

continuumGridStrat _ _ _ _ _ _ _ = fail "continuumGridStrat: Not enough domain splits provided!"

majorIterationStrat :: Config -> [DDom] -> TDom -> [UVDom]
                   -> Flow Index -> Flow Vis -> Flow Image
                   -> Strategy (Flow Image, Flow Image)
majorIterationStrat cfg ddom_s tdom uvdom_s ixs vis mod = do

  -- Calculate model grid using FFT (once)
  let gpar = cfgGrid cfg
      gcfpar = cfgGCF cfg
  bindRule dft $ regionKernel (head ddom_s) $ fftKern gpar (head uvdom_s)
  calculate (dft mod)

  -- Do continuum gridding for degridded visibilities. The actual
  -- degridding will be done in the inner loop, see continuumGridStrat.
  let vis' = degridModel vis mod
  continuumGridStrat cfg ddom_s tdom uvdom_s ixs vis vis'

  -- Clean
  let cpar = cfgClean cfg
  bindRule (\psfImg img mod -> splitModel $ clean psfImg img mod) $
    regionKernel (head ddom_s) $ cleanModel gpar cpar
  bindRule (\psfImg img mod -> splitResidual $ clean psfImg img mod) $
    regionKernel (head ddom_s) $ const $ cleanResidual gpar cpar

  return (loopIter vis mod,
          finalLoopIter vis mod)


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
  let ddom_s = [ddomss, ddoms, ddom]

  -- Data flows we want to calculate
  vis <- uniq $ flow "vis" ixs

  -- Create ranged domains for grid coordinates
  let gpar = cfgGrid cfg
  udoms <- makeRangeDomain 0 (gridWidth gpar)
  vdoms <- makeRangeDomain 0 (gridHeight gpar)

  -- Split and distribute over coordinate domain
  vdom <- split vdoms (gridTiles gpar)
  udom <- split udoms (gridTiles gpar)
  let uvdom_s = [(udoms, vdoms), (udom, vdom)]

  -- Compute PSF
  psfFlow <- continuumGridStrat cfg ddom_s tdom uvdom_s ixs vis (psfVis vis)
  void $ bindNew $ regionKernel ddomss $ imageWriter gpar "psf.img" psfFlow

  -- Major loops
  let start = (createImage, createImage)
  (model, residual) <- (\f -> foldM f start [1..10]) $ \(mod, _res) i -> do

    -- Calculate/create model
    bindRule createImage $ regionKernel ddomss $ imageInit gpar
    when (i > 1) $ calculate createImage -- workaround
    calculate mod

    -- Run major loop iteration
    majorIterationStrat cfg ddom_s tdom uvdom_s ixs vis mod

  -- Write out model grid
  bind createImage $ regionKernel ddomss $ imageInit gpar
  void $ bindNew $ regionKernel ddomss $
     imageWriter gpar (cfgOutput cfg ++ ".mod") model
  bind createImage $ regionKernel ddomss $ imageInit gpar
  void $ bindNew $ regionKernel ddomss $
     imageWriter gpar (cfgOutput cfg ++ ".res") residual

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
        , cleanThreshold = 4.0
        , cleanCycles    = 10
        }
      config = defaultConfig
        { cfgInput  = [ OskarInput "test_p00_s00_f00.vis" 2 2
                      , OskarInput "test_p00_s00_f01.vis" 2 2
                      ]
        , cfgPoints = 32131 * 200
        , cfgLoops  = 10
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
