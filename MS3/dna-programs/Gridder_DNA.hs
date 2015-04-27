{-# LANGUAGE
      CPP
    , TemplateHaskell
  #-}

module Main where

import Data.List
import Text.Printf

import System.IO

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

mkActorForSortedData :: (String -> TaskData -> DNA ()) -> Actor (String, String) ()
mkActorForSortedData f = actor $ \(ns_in, ns_out) -> do
  taskData <- readTaskDataP ns_in
  tdSorted <- liftIO $ mkSortedClone NormSort taskData
  f ns_out tdSorted
  liftIO $ finalizeSortedClone tdSorted >> finalizeTaskData taskData

runCPUGridderOnSavedDataWithSorting, optRomeinFullGCFOnSavedData :: Actor (String, String) ()
runCPUGridderOnSavedDataWithSorting = mkActorForSortedData (mkGcfAndCpuGridder True True True)
-- Use dedicated actor ATM
optRomeinFullGCFOnSavedData = mkActorForSortedData doIt
  where
    doIt ns_out tdSorted = runGridderWith $(mkStaticClosure 'optRomeinFullGCF) tdSorted "" ns_out

remotable [
    'runGatherGridderOnSavedData
  , 'runCPUGridderOnSavedDataWithSorting
  , 'optRomeinFullGCFOnSavedData
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

rep :: String -> DNA ()
rep s = liftIO $ print s >> hFlush stdout

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
      {-
      locRomeinShell <- startActor (N 0) (useLocal >> return $(mkStaticClosure 'runGridderOnLocalData))
      let
        sendToLocalAndDelay p = sendParam p locRomeinShell >> delay Local locRomeinShell
      locRomein <- sendToLocalAndDelay (head locs)
       -}
      rep "Here1"
      --
      mapM_ (uncurry writeTaskDataP) $ zip ns_loc taskData
      --
      let simpleRomeinUsingHalfOfFullGCFClo = $(mkStaticClosure 'simpleRomeinUsingHalfOfFullGCF)
      remRomeinUseHalf <- mapM (remActor romeinRemote) $ zip3 ns_loc ns_rem1 (repeat simpleRomeinUsingHalfOfFullGCFClo)
      rep "Here2"
      --
      let rems = zip3 (drop 4 ns_loc) (drop 4 ns_loc) simpleRomeinFullGCFClos
      remRomeinFull <- mapM (remActor romeinRemote) rems
      rep "Here3"
      --
      remGather <- mapM (remActor $(mkStaticClosure 'runGatherGridderOnSavedData)) $ zip4 ns_loc nst ns_rem2 (repeat False)
      rep "Here4"
      --
      remCPUSorted <- mapM (remActor $(mkStaticClosure 'runCPUGridderOnSavedDataWithSorting)) $ zip ns_loc ns_loc_cpus
      rep "Here5"
      -- Local actor executed sequentially
      -- await locRomein
      await locRomeinFull
      rep "Here6"
      -- mapM_ ((>>= await) . sendToLocalAndDelay) (tail locs)
      mapM_ ((>>= await) . startLoc) (tail locs)
      rep "Here7"
      mapM_ await $ remRomeinFull ++ remRomeinUseHalf ++ remGather ++ remCPUSorted
      rep "Here8"
  where
    romeinRemote = $(mkStaticClosure 'runGridderOnSavedData)
    rt = GridderActors.__remoteTable
       . Main.__remoteTable
    -- datasets = [pr p s f | p <- [0, 1], s <- [0] {- ..2] -}, f <- [0,1]]
    -- datasets = [pr p s f | p <- [0, 1], s <- [0..2], f <- [0,1]]
    datasets = take 4 [pr p s f | p <- [0, 1], s <- [0..2], f <- [0,1]]
    pr :: Int -> Int -> Int -> String
    pr = printf "test_p%02d_s%02d_f%02d.vis"
