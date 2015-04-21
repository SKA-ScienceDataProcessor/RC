{-# LANGUAGE
      CPP
    , TemplateHaskell
  #-}

module Main where

import Data.List
import Text.Printf
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Static (Closure)

import DNA

import OskarBinReader
import Namespace
import GridderActors

-- We have only 2 gather gridder variants at the moment and the selection is determined
--   by how we bin data -- for full or half gcf usage
runGatherGridderOnSavedData :: Actor (String, String, String, Bool) ()
runGatherGridderOnSavedData = actor $ \(ns_in, ns_tmp, ns_out, useHalfGcf) -> do
  taskData <- readTaskDataP ns_in
  binAndPregrid ns_tmp useHalfGcf taskData
  runGridderWith (if useHalfGcf then hc else fc) taskData ns_tmp ns_out
  liftIO $ finalizeTaskData taskData
  where
     hc = $(mkStaticClosure 'gatherGridderActorHalfGcf)
     fc = $(mkStaticClosure 'gatherGridderActorFullGcf)

runCPUGridderOnSavedDataWithSorting :: Actor (String, String) ()
runCPUGridderOnSavedDataWithSorting = actor $ \(ns_in, ns_out) -> do
  taskData <- readTaskDataP ns_in
  tdSorted <- liftIO $ mkSortedClone NormSort taskData
  mkGcfAndCpuGridder True True True ns_out tdSorted
  liftIO $ finalizeSortedClone tdSorted >> finalizeTaskData taskData

remotable [
    'runGatherGridderOnSavedData
  , 'runCPUGridderOnSavedDataWithSorting
  ]

remActor, locActor :: (Serializable a, Serializable b) => Closure (Actor a b) -> a -> DNA (Promise b)
remActor clo par = do
  shell <- startActor (N 1) (return clo)
  sendParam par shell
  delay Remote shell
locActor clo par = do
  shell <- startActor (N 0) (useLocal >> return clo)
  sendParam par shell
  delay Local shell

main :: IO ()
main = do
    -- Don't fuse these ATM
    nst <- mapM (createNameSpace RAM) datasets
    ns_loc <- mapM (createNameSpace Persistent) datasets
    ns_rem1 <- mapM (`addNameSpace` "from_1") ns_loc
    ns_rem2 <- mapM (`addNameSpace` "from_2") ns_loc
    ns_loc_cpus <- mapM (`addNameSpace` "from_CPU_s") ns_loc
    dnaRun rt $ do
      -- Don't fuse also
      taskData <- mapM binReader datasets
      --
      let
        simpleRomeinFullGCFClos = repeat $(mkStaticClosure 'simpleRomeinFullGCF)
        -- 4 tasks will run locally, remaining 8 -- remotely
        locs = take 4 $ zip3 ns_loc taskData simpleRomeinFullGCFClos
        startLoc = locActor $(mkStaticClosure 'runGridderOnLocalData)
      locRomeinFull <- startLoc (head locs)
      --
      mapM_ (uncurry writeTaskDataP) $ zip ns_loc taskData
      --
      let simpleRomeinUsingHalfOfFullGCFClo = $(mkStaticClosure 'simpleRomeinUsingHalfOfFullGCF)
      remRomeinUseHalf <- mapM (remActor romeinRemote) $ zip3 ns_loc ns_rem1 (repeat simpleRomeinUsingHalfOfFullGCFClo)
      --
      let rems = zip3 (drop 4 ns_loc) (drop 4 ns_loc) simpleRomeinFullGCFClos
      remRomeinFull <- mapM (remActor romeinRemote) rems
      --
      remGather <- mapM (remActor $(mkStaticClosure 'runGatherGridderOnSavedData)) $ zip4 ns_loc nst ns_rem2 (repeat False)
      --
      remCPUSorted <- mapM (remActor $(mkStaticClosure 'runCPUGridderOnSavedDataWithSorting)) $ zip ns_loc ns_loc_cpus
      -- Local actor executed sequentially
      await locRomeinFull
      mapM_ ((>>= await) . startLoc) (tail locs)
      mapM_ await $ remRomeinFull ++ remRomeinUseHalf ++ remGather ++ remCPUSorted
  where
    romeinRemote = $(mkStaticClosure 'runGridderOnSavedData)
    rt = GridderActors.__remoteTable
       . Main.__remoteTable
    -- datasets = [pr p s f | p <- [0, 1], s <- [0] {- ..2] -}, f <- [0,1]]
    -- datasets = [pr p s f | p <- [0, 1], s <- [0..2], f <- [0,1]]
    datasets = take 4 [pr p s f | p <- [0, 1], s <- [0..2], f <- [0,1]]
    pr :: Int -> Int -> Int -> String
    pr = printf "test_p%02d_s%02d_f%02d.vis"
