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

-- Magic function which only used here to avoid problems with TH
closure :: a -> Closure a
closure = undefined

workerActor :: Actor (FreqCh,Int) Image
workerActor = actor $ \_ ->
    error "Some kind of magic here"

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
    await =<< delay collector

main :: IO ()
main = do
  return ()
