{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | High level dataflow for imaging program
module Main where

import Control.Distributed.Process (Closure)
import DNA
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Frequency channel dummy type currently
newtype FreqCh = FreqCh Int
               deriving (Show,Binary,Typeable)

-- | Dummy type for clean image
newtype Image = Image Int
              deriving (Show,Binary,Typeable)


scheduleFreqCh
    :: [(FreqCh,Double)]
    -> Int
    -> [(FreqCh,Int)]
    -> [(FreqCh,Int)]
scheduleFreqCh = undefined



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
--
-- Data types at this point are just names taken from dataflow
-- diagrams.
----------------------------------------------------------------


-- | Compound gridder actor.
--
--   We want to store and reuse GCF object which is not
gridderActor :: Actor (BinnedViz, GCF) DirtyImage
gridderActor = actor $ \(viz,gcf) -> do
    -- Here dataflow is very simple and linear and easiest way to
    -- sequence computations is with eval.
    --
    -- FIXME: Maybe we want to ensure that actor spawned during by
    --        eval'd actor should terminate after eval?
    --
    -- FIMXE: actors mentioned here are not described yet
    grid       <- eval realGridder (viz,gcf)
    dirtyImage <- eval actorFFT grid
    return dirtyImage


imagingActor :: Actor FreqCh CleanImage
imagingActor = Actor $ \freqCh -> do
    -- I assume that we only need to calculate GCF once
    gcf <- getGCFsomehow freqCh
    -- * PSF calculations
    --
    -- FIXME: It's not clear whether calculations of PSF and dirty
    --        images could be interleaved. Probably no because of
    --        memory constraints so I use eval here.
    --
    --        In theory this is task for scheduler which should decide
    --        whether to to run tasks in parallel or sequentially
    --        based on memory/CPU/GPU constraints
    psfViz <- getPsfVizibilitiesSomehow
    pfs    <- eval gridderActor (psfViz,gcf)
    -- Obtain vizibilities
    viz        <- getVizibilitiesInSomeOtherWay freqCh
    -- Now we can start cleaning loop. We could express it using
    -- ordinary recursion.
    let loop v = do
            dirtyImage <- eval gridderActor (viz,gcf)
            cleanImage <- eval cleanActor   dirtyImage
            if cleanImage is goodEnough
               then return cleanImage
               else do predViz <- eval degridActor cleanImage
                       viz'    <- eval subtractActor (viz,predViz)
                       loop viz'
    loop viz


----------------------------------------------------------------
-- High level dataflow
----------------------------------------------------------------

-- | Actor which generate N clean images for given frequency channel
workerActor :: Actor (FreqCh,Int) Image
workerActor = actor $ \_ ->
    error "Some kind of magic here"

-- | Actor which collects images in tree-like fashion
imageCollector :: TreeCollector Image
imageCollector = undefined

mainActor :: Actor [(FreqCh,Int)] Image
mainActor = actor $ \input -> do
    -- Start worker actors
    workers <- startGroup (Frac 1) (NNodes 1) $ do
        useLocal
        return (closure workerActor)
    -- Obtain estimates using some secret technique
    estimates <- undefined "Not implemented and unsure how to store it"
    distributeWork input (scheduleFreqCh estimates) workers
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
