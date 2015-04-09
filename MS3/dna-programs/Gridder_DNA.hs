{-# LANGUAGE
      CPP
    , TemplateHaskell
  #-}

module Main where

import Control.Distributed.Static (Closure)

import Namespace
import OskarBinReader
import GCF
import GPUGridder

import GridderActors

import DNA

saveDataAndRunRemote :: Actor(String, String, TaskData, Closure (Actor (TaskData, GCF) Grid)) ()
saveDataAndRunRemote = actor $ \(ns, ns_out, td, gridderClosure) -> do
  eval writeTaskDataActor (ns, td)
  shellRG <- startActor (N 1) $ return $(mkStaticClosure 'runGridderOnSavedData)
  sendParam (ns, ns_out, gridderClosure) shellRG
  futRG <- delay Remote shellRG
  await futRG

remotable [
    'saveDataAndRunRemote
  ]

-- Simple sequential 1-node program to test if
--   all parts work together.
main :: IO ()
main = do
    nst <- createNameSpace RAM dataset
    ns_loc <- createNameSpace Persistent dataset
    ns_rem <- addNameSpace ns_loc "from_1"
    dnaRun rt $ do
      taskData <- eval binReaderActor dataset
      -- Now we locally start actor which writes converted
      --  data do tmp location and start remote actor using these data
      -- but writing result back to persisten location
      shellSDRR <- startActor (N 0) $ useLocal >> return $(mkStaticClosure 'saveDataAndRunRemote)
      shellLoc <- startActor (N 0) $ useLocal >> return $(mkStaticClosure 'runGridderOnLocalData)
      sendParam (nst, ns_rem, taskData, $(mkStaticClosure 'simpleRomeinUsingHalfOfFullGCF)) shellSDRR
      sendParam (ns_loc, taskData, $(mkStaticClosure 'simpleRomeinFullGCF)) shellLoc
      futSDRR <- delay Local shellSDRR
      futLoc <- delay Local shellLoc
      await futSDRR
      await futLoc
  where
    rt = GridderActors.__remoteTable
       . Main.__remoteTable
    dataset = "test_p00_s00_f00.vis"