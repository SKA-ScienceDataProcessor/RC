{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | High level dataflow for imaging program
module Main where

import Control.Distributed.Process (Closure)
import Control.Monad

import DNA
import DNA.Channel.File

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe      (fromMaybe)
import Data.Time.Clock

import Config
import Data
import Kernel
import Oskar
import Scheduling
import Vector

----------------------------------------------------------------
-- Imaging dataflow
--
-- It's implicitly assumed that all dataflow for generating single
-- image is confined to single computer.
----------------------------------------------------------------


-- | Compound gridding actor.
gridderActor :: GridPar -> GCFPar -> GridKernel -> DFTKernel
             -> Actor (Vis, GCFSet) Image
gridderActor gpar gcfpar gridk dftk = actor $ \(vis,gcfSet) -> do

    -- Grid visibilities to a fresh uv-grid
    grid <- kernel "grid" [] $ liftIO $ do
      grid <- gridkCreateGrid gridk gpar gcfpar
      gridkGrid gridk vis gcfSet grid

    -- Transform uv-grid into an (possibly dirty) image
    kernel "ifft" [] $ liftIO $ do
      dftIKernel dftk grid

-- | Compound degridding actor.
degridderActor :: GridKernel -> DFTKernel
             -> Actor (Image, Vis, GCFSet) Vis
degridderActor gridk dftk = actor $ \(model,vis,gcfSet) -> do

    -- Transform image into a uv-grid
    grid <- kernel "fft" [] $ liftIO $ do
      dftKernel dftk model

    -- Degrid to obtain new visibilitities for the positions in "vis"
    kernel "degrid" [] $ liftIO $ do
      gridkDegrid gridk grid gcfSet vis

imagingActor :: Config -> Actor DataSet Image
imagingActor cfg = actor $ \dataSet -> do

    -- Copy data set locally
    oskarChan <- createFileChan Local "oskar"
    (gridk, dftk, cleank, vis0, psfVis, gcfSet) <- unboundKernel "setup" [] $ liftIO $ do
      transferFileChan (dsData dataSet) oskarChan "data.vis"

      -- Initialise our kernels
      gcfk <- initKernel gcfKernels (cfgGCFKernel cfg)
      gridk <- initKernel gridKernels (cfgGridKernel cfg)
      dftk <- initKernel dftKernels (cfgDFTKernel cfg)
      cleank <- initKernel cleanKernels (cfgCleanKernel cfg)

      -- Read input data from Oskar
      vis <- readOskar oskarChan "data.vis" (dsChannel dataSet) (dsPolar dataSet)
      psfVis <- constVis 1 vis

      -- Run GCF kernel to generate GCFs
      gcfSet <- gcfKernel gcfk (cfgGridPar cfg) (cfgGCFPar cfg)
                               (visMinW vis) (visMaxW vis)

      -- Let grid kernel prepare for processing GCF and visibilities
      -- (transfer buffers, do binning etc.)
      vis' <- gridkPrepareVis gridk vis
      psfVis' <- gridkPrepareVis gridk psfVis
      gcfSet' <- gridkPrepareGCF gridk gcfSet

      -- Calculate PSF using the positions from Oskar data
      return (gridk, dftk, cleank,
              vis', psfVis', gcfSet')

    -- Calculate PSF
    let gridAct = gridderActor (cfgGridPar cfg) (cfgGCFPar cfg) gridk dftk
        degridAct = degridderActor gridk dftk
    psf <- eval gridAct (psfVis,gcfSet)

    -- Major cleaning loop. We always do the number of configured
    -- iterations.
    let majorLoop i vis = do
         -- Generate the dirty image from the visibilities
         dirtyImage <- eval gridAct (vis,gcfSet)

         -- Clean the image
         (residual, model) <- kernel "clean" [] $ liftIO $
           cleanKernel cleank (cfgCleanPar cfg) dirtyImage psf

         -- Done with the loop?
         if i >= cfgMajorLoops cfg then return residual else do

           -- We continue - residual isn't needed any more
           kernel "free" [] $ liftIO $ freeVector (imgData residual)
           -- De-grid the model
           mvis <- eval degridAct (model,vis,gcfSet)
           -- Loop
           vis' <- kernel "subtract" [] $ liftIO $ subtractVis vis mvis
           majorLoop (i+1) vis'

    -- Run above loop. The residual of the last iteration is the
    -- result of this actor
    res <- majorLoop 1 vis0

    -- Free GCFs
    kernel "clean cleanup" [] $ liftIO $ do
        forM_ (gcfs gcfSet) $ \gcf ->
            freeVector (gcfData gcf)

    -- More Cleanup? Eventually kernels will probably want to do
    -- something here...

    return res

----------------------------------------------------------------
-- High level dataflow
----------------------------------------------------------------

-- | Actor which generate N clean images for given frequency channel
workerActor :: Actor (Config, DataSet) (FileChan Image)
workerActor = actor $ \(cfg, dataSet) -> do

    -- Initialise image sum
    img0 <- kernel "init image" [] $ liftIO $ constImage (cfgGridPar cfg) 0
    let loop i img | i >= dsRepeats dataSet  = return img
                   | otherwise = do
           -- Generate image, sum up
           img' <- eval (imagingActor cfg) dataSet
           unboundKernel "addImage" [] $ liftIO $
             addImage img img'

    -- Run the loop
    img <- loop 0 img0

    -- Allocate file channel for output
    outChan <- createFileChan Remote "image"
    kernel "write image" [] $ liftIO $ writeImage img outChan "data.img"
    return outChan

-- | Actor which collects images in tree-like fashion
imageCollector :: TreeCollector (FileChan Image)
imageCollector = undefined

-- | A measure for the complexity of processing a data set.
type Weight = Rational

-- | Actor to estimate the cost of doing a single repeat on the given
-- data set using our configuration. This basically means running the
-- whole imaging pipeline once, measuring the performance it takes.
--
-- TODO: Warmup? Determine mean derivation?
estimateActor :: Actor (Config, DataSet) (DataSet, Weight)
estimateActor = actor $ \(cfg, dataSet) -> do

    -- Generate image, measuring time
    start <- unboundKernel "estimate start" [] $ liftIO getCurrentTime
    void $ eval (imagingActor cfg) dataSet
    end <- unboundKernel "estimate end" [] $ liftIO getCurrentTime

    -- Return time taken
    return (dataSet, toRational $ end `diffUTCTime` start)

remotable
  [ 'workerActor
  --, 'imageCollector -- doesn't work yet?
  , 'estimateActor
  ]

-- | The main program actor. Schedules the given data sets
-- appropriately to the available nodes.
mainActor :: Actor (Config, [DataSet]) (FileChan Image)
mainActor = actor $ \(cfg, dataSets) -> do

    -- Check that we actually have enough nodes. The local node counts.
    let setCount = length dataSets
    avail <- availableNodes
    when (setCount > avail + 1) $
        fail $ "Not enough nodes: Require " ++ show setCount ++ " to run data sets!"

    -- Allocate estimation nodes
    estimateWorkers <- startGroup (N (setCount-1)) (NNodes 1) $ do
        useLocal
        return $(mkStaticClosure 'estimateActor)
    distributeWork dataSets (const (map ((,) cfg))) estimateWorkers

    -- Run estimation, collect weighted data sets
    grp <- delayGroup estimateWorkers
    weightedDataSets <- gather grp (flip (:)) []

    -- Now start worker actors
    waitForResoures estimateWorkers
    workers <- startGroup (Frac 1) (NNodes 1) $ do
        useLocal
        return $(mkStaticClosure 'workerActor)

    -- Schedule work
    let schedule = balancer avail (map (fromRational . snd) weightedDataSets)
        splitData low high dataSet =
            let atRatio r = floor $ r * fromIntegral (dsRepeats dataSet)
            in dataSet { dsRepeats = atRatio high - atRatio low }
    distributeWork
        (map fst weightedDataSets)
        (const $ map ((,) cfg) . distributer splitData schedule)
        workers

    -- Spawn tree collector
    --
    -- This is attempt to encapsulate tree-like reduction in single
    -- actor. In hierarchical DDP tree-like reduction was defined by
    -- spawning actors in tree.
    collector <- startCollectorTree undefined undefined $ do
        useLocal
        return undefined -- $(mkStaticClosure 'treeCollector) -- see above
    connect workers collector
    await =<< delay Remote collector

main :: IO ()
main = dnaRun rtable $ do
    -- We expect configuration in our working directory
    (datafiles, Just config) <- unboundKernel "configure" [] $ liftIO $ do
        !datafiles <- fmap decode $ LBS.readFile "data.cfg" :: IO (Maybe [(String, Int)])
        !config    <- fmap decode $ LBS.readFile "imaging.cfg" :: IO (Maybe Config)
        return (datafiles, config)

    -- Create Oskar file channels
    dataSets <- fmap concat $ forM (fromMaybe [] datafiles) $ \(file, repeats) -> do
        chan <- createFileChan Remote "oskar"

        -- Import file, and determine the used frequency channels and
        -- polarisations
        (polars, freqs) <- unboundKernel "import oskar" [] $ liftIO $ do
            importToFileChan chan "data.vis" file
            readOskarHeader chan "data.vis"

        -- Interpret every combination as a data set
        return [ DataSet { dsData    = chan
                         , dsChannel = freq
                         , dsPolar   = polar
                         , dsRepeats = repeats
                         }
               | freq <- freqs, polar <- [XX] ]

    -- Execute main actor
    chan <- eval mainActor (config, dataSets)

    -- Copy result image to working directory
    unboundKernel "export image" [] $ liftIO $
        exportFromFileChan chan "data.img" "output.img"
  where
    rtable = __remoteTable
