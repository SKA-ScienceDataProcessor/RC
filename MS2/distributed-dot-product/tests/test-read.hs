-- |test-read.hs
--
-- Test file read actor from DNA.
--
-- Copyright (C) 2014 Braam Research

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forM_, forM, when)
import Control.Monad.Trans
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)

import System.Exit (exitFailure)

import Test.HUnit

master :: MVar (Maybe String) -> [NodeId] -> Process ()
master resultMVar _nodes = do
        return ()

slave :: Process ()
slave = do
        return ()

remotable ['slave]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable

failure :: String -> IO a
failure msg = do
        putStrLn $ "Failure reason: " ++ msg
        exitFailure

mainTest :: IO ()
mainTest = do
        flag <- newEmptyMVar
        forkIO $ do
                backend <- initializeBackend "localhost" "12345" rtable
                startMaster backend (master flag)
        threadDelay 5000000
        result <- tryTakeMVar flag
        case result of
                Nothing -> failure "timeout"
                Just Nothing -> return ()
                Just (Just reason) -> failure reason

main = do
        mainTest
