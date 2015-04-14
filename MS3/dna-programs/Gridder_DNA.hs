{-# LANGUAGE
      CPP
    , TemplateHaskell
  #-}

module Main where

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

remotable [
    'runGatherGridderOnSavedData
  ]


main :: IO ()
main = do
    nst <- createNameSpace RAM dataset
    ns_loc <- createNameSpace Persistent dataset
    ns_rem1 <- addNameSpace ns_loc "from_1"
    ns_rem2 <- addNameSpace ns_loc "from_2"
    ns_loc_cpu <- addNameSpace ns_loc "from_CPU"
    dnaRun rt $ do
      taskData <- binReader dataset
      --
      shellLoc <- startActor (N 0) $ useLocal >> return $(mkStaticClosure 'runGridderOnLocalData)
      sendParam (ns_loc, taskData, $(mkStaticClosure 'simpleRomeinFullGCF)) shellLoc
      --
      writeTaskDataP ns_loc taskData
      --
      shellRG <- startActor (N 1) $ return $(mkStaticClosure 'runGridderOnSavedData)
      sendParam (ns_loc, ns_rem1, $(mkStaticClosure 'simpleRomeinUsingHalfOfFullGCF)) shellRG
      --
      shellGG <- startActor (N 2) $ return $(mkStaticClosure 'runGatherGridderOnSavedData)
      sendParam (ns_loc, nst, ns_rem2, False) shellGG
      --
      futRG <- delay Remote shellRG
      futGG <- delay Remote shellGG
      futLoc <- delay Local shellLoc

      await futLoc
      eval (mkGcfAndCpuGridderActor True True False) (ns_loc_cpu, taskData)
      liftIO $ finalizeTaskData taskData

      await futRG
      await futGG
  where
    rt = GridderActors.__remoteTable
       . Main.__remoteTable
    dataset = "test_p00_s00_f00.vis"
