{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TypeOperators,
             FlexibleContexts #-}

module Main where

import Control.Monad

import Data.List
import qualified Data.Map as Map
import Data.Yaml

import Flow
import Flow.Builder ( rule )
import Flow.Kernel
import Flow.Domain ( regionBins )

import Kernel.Binning
import Kernel.Cleaning
import Kernel.Data
import Kernel.Degrid
import Kernel.Facet
import Kernel.FFT
import Kernel.Gridder
import Kernel.IO
import Kernel.Scheduling
import Kernel.Utils

import System.Environment
import System.Directory
import System.FilePath

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
facetSum :: Flow Image -> Flow Image -> Flow Image
facetSum = flow "facet sum"
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
summed vis0 vis = sumImage (facetSum (gridder vis0 vis) createImage) createImage

-- | Calculate a point spread function, which tells us the image shape
-- of a single point source in the middle of the field of view
psf :: Flow Vis -> Flow Image
psf vis = summed vis (psfVis vis)

-- | Update a model by gridding the given (possibly corrected)
-- visibilities and cleaning the result.
model :: Flow Vis -> Flow Vis -> Flow Image -> Flow Image
model vis0 vis mdl = splitModel $ clean (psf vis0) (summed vis0 vis) mdl

-- | Calculate a residual by gridding the given visibilities and
-- cleaning the result.
residual :: Flow Vis -> Flow Vis -> Flow Image
residual vis0 vis = splitResidual $ clean (psf vis0) (summed vis0 vis) createImage

-- | Degrid a model, producing corrected visibilities where we
-- have attempted to eliminate the effects of sources in the model.
degridModel :: Flow Vis -> Flow Image -> Flow Vis
degridModel vis mdl = degrid (gcf vis) (dft mdl) vis

-- | Major loop iteration: From visibilities infer components in the
-- image and return the updated model.
loopIter :: Flow Vis -> Flow Image -> Flow Image
loopIter vis mdl = model vis (degridModel vis mdl) mdl
-- | Final major loop iteration: Do the same steps as usual, but
-- return just the residual.
finalLoopIter :: Flow Vis -> Flow Image -> Flow Image
finalLoopIter vis mdl = residual vis (degridModel vis mdl)

-- ----------------------------------------------------------------------------
-- ---                               Strategy                               ---
-- ----------------------------------------------------------------------------

-- | Implement one major iteration of continuum gridding (@loopIter@) for
-- the given input 'Flow's over a number of datasets given by the data
-- set domains @ddoms@. Internally, we will distribute twice over
-- @DDom@ and once over @UVDom@.
continuumGridStrat :: Config -> [DDom] -> TDom -> [UVDom] -> [LMDom]
                   -> Flow Index -> Flow Vis -> Flow Vis
                   -> Strategy (Flow Image)
continuumGridStrat cfg [ddomss,ddoms,ddom] tdom [uvdoms,uvdom] [_lmdoms,lmdom]
                   ixs vis0 vis
 = implementing (summed vis0 vis) $ do

  -- Helpers
  let dkern :: IsKernelDef kf => kf -> kf
      dkern = regionKernel ddom
      gpar = cfgGrid cfg
      gcfpar = cfgGCF cfg
      gcfsiz = gcfSize gcfpar
      numOps = cfgPointsIn cfg * gcfsiz * gcfsiz

  -- Intermediate Flow nodes
  let gridded = grid vis (gcf vis0) createGrid -- grid from vis
      images = facetSum (idft gridded) createImage
      summed' = summed vis0 vis  -- images, summed over channels

  -- Distribute over nodes
  distribute ddoms ParSchedule $ do

    -- Loop over data sets
    distribute ddom SeqSchedule $ do

      -- Loop over facets
      distribute (fst lmdom) SeqSchedule $ distribute (snd lmdom) SeqSchedule $ do
        let fkern :: IsKernelDef kf => kf -> kf
            fkern = regionKernel (fst lmdom) . regionKernel (snd lmdom)
            rkern :: IsKernelDef kf => kf -> kf
            rkern = dkern . fkern

        -- Read in visibilities
        rebind ixs $ scheduleSplit ddomss ddom
        bind vis0 $ oskarReader ddom tdom (cfgInput cfg) 0 0 ixs

        -- Create w-binned domain, split
        wdoms <- makeBinDomain $ dkern $ binSizer gpar tdom uvdom vis0
        wdom <- split wdoms (gridBins gpar)

        -- Loop over tiles
        distribute (snd uvdom) SeqSchedule $ distribute (fst uvdom) SeqSchedule $ do

          -- Load GCFs
          bindRule gcf $ rkern $ const $ gcfKernel gcfpar wdom
          distribute wdom SeqSchedule $ calculate $ gcf vis0

          -- Rotate visibilities
          rebind vis0 $ dkern $ rotateKernel cfg lmdom tdom

          -- Bin visibilities (could distribute, but there's no benefit)
          rebind vis0 $ rkern $ binner gpar tdom uvdom wdom

          -- Degrid / generate PSF (depending on vis)
          rule degrid $ \(gcfs :. uvgrid :. vis' :. Z) -> do
            rebind uvgrid $ distributeGrid ddomss ddom lmdom uvdoms
            let (degridkern, degridhint) = selectDegridKernel (cfgGridderType cfg)
            bind (degrid gcfs uvgrid vis') $ rkern $ hints [setDblOpts (8 * numOps) degridhint] $
              degridkern gpar gcfpar uvdom wdom uvdoms gcfs uvgrid vis'
          bindRule psfVis $ rkern $ psfVisKernel uvdom wdom
          calculate vis

          -- Gridding
          bind createGrid $ rkern $ gridInit gcfpar uvdom
          let (gridkern, gridhint) = selectGridKernel (cfgGridderType cfg)
              binSize (_,_,s) = s
              hint (visRegs:_) = [setDblOpts (8 * ops * gcfsiz * gcfsiz) gridhint]
                where wBinReg = (!!5) -- d, l, m, u, v, w - we want region six
                      ops = sum $ map binSize $ concatMap (regionBins . wBinReg) visRegs
          bindRule grid $ rkern $ hintsByPars hint $ gridkern gpar gcfpar uvdoms wdom uvdom
          calculate gridded

        -- Compute the result by detiling & iFFT on tiles
        bind createGrid $ rkern $ gridInitDetile uvdoms
        bind gridded $ rkern $ gridDetiling gcfpar uvdom uvdoms gridded createGrid
        bindRule idft $ rkern $ ifftKern gpar uvdoms
        calculate $ idft gridded

      -- Sum up facets
      bind createImage $ dkern $ imageInit gpar
      bind images $ dkern $ imageDefacet gpar lmdom (idft gridded) createImage

    -- Sum up images locally
    bind createImage $ regionKernel ddoms $ imageInit gpar
    bindRule sumImage $ imageSum gpar ddom ddoms
    calculate summed'

  -- Sum up images over nodes
  recover summed' $ regionKernel ddoms $ imageInit gpar
  bind createImage $ regionKernel ddomss $ imageInit gpar
  bind summed' $ imageSum gpar ddoms ddomss summed' createImage

continuumGridStrat _ _ _ _ _ _ _ _ = fail "continuumGridStrat: Not enough domain splits provided!"

majorIterationStrat :: Config -> [DDom] -> TDom -> [UVDom] -> [LMDom]
                   -> Flow Index -> Flow Vis -> Flow Image
                   -> Strategy (Flow Image, Flow Image)
majorIterationStrat cfg ddom_s tdom uvdom_s lmdom_s ixs vis mdl = do

  -- Calculate model grid using FFT (once)
  let gpar = cfgGrid cfg
  bindRule dft $ regionKernel (head ddom_s) $ fftKern gpar (head uvdom_s)
  calculate (dft mdl)

  -- Do continuum gridding for degridded visibilities. The actual
  -- degridding will be done in the inner loop, see continuumGridStrat.
  let vis' = degridModel vis mdl
  void $ continuumGridStrat cfg ddom_s tdom uvdom_s lmdom_s ixs vis vis'

  -- Clean
  let cpar = cfgClean cfg
  bindRule (\psfImg img mdl' -> splitModel $ clean psfImg img mdl') $
    regionKernel (head ddom_s) $ cleanModel gpar cpar
  bindRule (\psfImg img mdl' -> splitResidual $ clean psfImg img mdl') $
    regionKernel (head ddom_s) $ const $ cleanResidual gpar cpar

  return (loopIter vis mdl,
          finalLoopIter vis mdl)


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

  -- Create ranged domains for image coordinates (split into facets)
  let gpar = cfgGrid cfg
  ldoms <- makeRangeDomain 0 (gridImageWidth gpar)
  mdoms <- makeRangeDomain 0 (gridImageHeight gpar)
  ldom <- split ldoms (gridFacets gpar)
  mdom <- split mdoms (gridFacets gpar)
  let lmdom_s = [(ldoms, mdoms), (ldom, mdom)]

  -- Create ranged domains for grid coordinates (split into tiles)
  udoms <- makeRangeDomain 0 (gridWidth gpar)
  vdoms <- makeRangeDomain 0 (gridHeight gpar)
  vdom <- split vdoms (gridTiles gpar)
  udom <- split udoms (gridTiles gpar)
  let uvdom_s = [(udoms, vdoms), (udom, vdom)]

  -- Compute PSF
  psfFlow <- continuumGridStrat cfg ddom_s tdom uvdom_s lmdom_s ixs vis (psfVis vis)
  void $ bindNew $ regionKernel ddomss $ imageWriter gpar "psf.img" psfFlow

  -- Major loops
  let start = (createImage, createImage)
  (finalMod, finalRes) <- (\f -> foldM f start [1..cfgLoops cfg]) $ \(mod', _res) i -> do

    -- Calculate/create model
    bindRule createImage $ regionKernel ddomss $ imageInit gpar
    when (i > 1) $ calculate createImage -- workaround
    calculate mod'

    -- Run major loop iteration
    majorIterationStrat cfg ddom_s tdom uvdom_s lmdom_s ixs vis mod'

  -- Write out model grid
  bind createImage $ regionKernel ddomss $ imageInit gpar
  void $ bindNew $ regionKernel ddomss $
     imageWriter gpar (cfgOutput cfg ++ ".mod") finalMod
  bind createImage $ regionKernel ddomss $ imageInit gpar
  void $ bindNew $ regionKernel ddomss $
     imageWriter gpar (cfgOutput cfg) finalRes

main :: IO ()
main = do

  -- Determine work directory (might be given on the command line)
  args <- getArgs
  dir <- case find ((== "--workdir") . fst) $ zip args (tail args) of
   Just (_, wdir) -> return wdir
   Nothing        -> getCurrentDirectory

  -- Read configuration
  let configFile = dir </> "continuum.yaml"
  putStrLn $ "Reading configuration from " ++ configFile
  m_config <- decodeFileEither configFile

  case m_config of
   Left err -> putStrLn $ "Failed to read configuration file: " ++ show err
   Right config -> do

    -- Show strategy - but only for the root process
    when (not ("--internal-rank" `elem` args)) $ do
      dumpSteps $ continuumStrat config
      putStrLn "----------------------------------------------------------------"
      putStrLn ""

    -- Execute strategy
    execStrategyDNA $ continuumStrat config
