{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | High level dataflow for imaging program
module Main where

import Control.Distributed.Process (Closure)
import Control.Monad   (void, when)

import DNA
import DNA.Channel.File

import Data.Binary     (Binary)
import Data.Time.Clock
import Data.Typeable   (Typeable)
import GHC.Generics    (Generic)

import Data
import Kernel
import Scheduling
import Vector

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

data OskarData

-- | A data set, consisting of an Oskar file name and the frequency
-- channel & polarisation we are interested in.
data DataSet = DataSet
  { dsData :: FileChan OskarData -- ^ Oskar data to read
  , dsChannel :: Int   -- ^ Frequency channel to process
  , dsPolar :: Polar   -- ^ Polarisation to process
  , dsRepeats :: Int   -- ^ Number of times we should process this
                       -- data set to simulate a larger workload
  }
  deriving (Generic,Typeable)
instance Binary DataSet

-- | Main run configuration. This contains all parameters we need for
-- runnin the imaging pipeline.
data Config = Config
  { cfgDataSets :: [DataSet] -- ^ Data sets to process
  , cfgGridPar :: GridPar    -- ^ Grid parameters to use. Must be compatible with used kernels!
  , cfgGCFPar :: GCFPar      -- ^ GCF parameters to use.

  , cfgGCFKernel :: String   -- ^ The GCF kernel to use
  , cfgGridKernel :: String  -- ^ The gridding kernel to use
  , cfgDFTKernel :: String   -- ^ The fourier transformation kernel to use
  , cfgCleanKernel :: String -- ^ The cleaning kernel to use

  , cfgMinorLoops :: Int     -- ^ Maximum number of minor loop iterations
  , cfgMajorLoops :: Int     -- ^ Maximum number of major loop iterations
  , cfgCleanGain :: Double   -- ^ Cleaning strength (?)
  , cfgCleanThreshold :: Double -- ^ Residual threshold at which we should stop cleanining
  }
  deriving (Generic,Typeable)
instance Binary Config

----------------------------------------------------------------
-- Actors
----------------------------------------------------------------

-- Magic function which only used here to avoid problems with TH It
-- pretends to create closure out of functions and allows to type
-- check code but not run it.
closure :: a -> Closure a
closure = undefined


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
      transferFileChan (dsData dataSet) oskarChan "data"

      -- Initialise our kernels
      gcfk <- initKernel gcfKernels (cfgGCFKernel cfg)
      gridk <- initKernel gridKernels (cfgGridKernel cfg)
      dftk <- initKernel dftKernels (cfgDFTKernel cfg)
      cleank <- initKernel cleanKernels (cfgCleanKernel cfg)

      -- Read input data from Oskar
      let readOskarData :: FileChan OskarData -> Int -> Polar -> IO Vis
          readOskarData = undefined
      vis <- readOskarData oskarChan (dsChannel dataSet) (dsPolar dataSet)
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
           cleanKernel cleank (cfgMinorLoops cfg) (cfgCleanGain cfg) (cfgCleanThreshold cfg)
                       dirtyImage psf

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

    -- Cleanup? Eventually kernels will probably want to do something
    -- here...

    return res

----------------------------------------------------------------
-- High level dataflow
----------------------------------------------------------------

-- | Actor which generate N clean images for given frequency channel
workerActor :: Config -> Actor DataSet (FileChan Image)
workerActor cfg = actor $ \dataSet -> do

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
    kernel "write image" [] $ liftIO $ writeImage img outChan "data"
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
estimateActor :: Config -> Actor DataSet (DataSet, Weight)
estimateActor cfg = actor $ \dataSet -> do

    -- Generate image, measuring time
    start <- unboundKernel "estimate start" [] $ liftIO getCurrentTime
    void $ eval (imagingActor cfg) dataSet
    end <- unboundKernel "estimate end" [] $ liftIO getCurrentTime

    -- Return time taken
    return (dataSet, toRational $ end `diffUTCTime` start)

-- | The main program actor. Schedules the given data sets
-- appropriately to the available nodes.
mainActor :: Config -> Actor [DataSet] (FileChan Image)
mainActor cfg = actor $ \dataSets -> do

    -- Check that we actually have enough nodes. The local node counts.
    let setCount = length dataSets
    avail <- availableNodes
    when (setCount > avail + 1) $
        fail $ "Not enough nodes: Require " ++ show setCount ++ " to run these data sets!"

    -- Allocate estimation nodes
    estimateWorkers <- startGroup (N setCount) (NNodes 1) $ do
        useLocal
        return (closure (estimateActor cfg))
    distributeWork dataSets (const id) estimateWorkers

    -- Run estimation, collect weighted data sets
    grp <- delayGroup estimateWorkers
    weightedDataSets <- gather grp (flip (:)) []

    -- Now start worker actors
    workers <- startGroup (Frac 1) (NNodes 1) $ do
        useLocal
        return (closure (workerActor cfg))

    -- Schedule work
    let schedule = balancer avail (map (fromRational . snd) weightedDataSets)
        splitData low high dataSet =
            let atRatio r = floor $ r * fromIntegral (dsRepeats dataSet)
            in dataSet { dsRepeats = atRatio high - atRatio low }
    distributeWork
        (map fst weightedDataSets)
        (const $ distributer splitData schedule)
        workers

    -- Spawn tree collector
    --
    -- This is attempt to encapsulate tree-like reduction in single
    -- actor. In hierarchical DDP tree-like reduction was defined by
    -- spawning actors in tree.
    collector <- startCollectorTree undefined undefined $ do
        useLocal
        return (closure imageCollector)
    connect workers collector
    await =<< delay Remote collector

main :: IO ()
main = do
  return ()
