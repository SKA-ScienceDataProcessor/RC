{-# LANGUAGE CPP, BangPatterns #-}

module Driver where

import Control.Monad
#ifdef AUTO
import Control.Concurrent.Async
#else
import Data.IORef
import Control.Concurrent
#endif

import GHC.RTS.Events
-- Bench
import Data.Time.Clock

import System.FilePath.Find

workSteal :: [IO a] -> IO [a]
#ifdef AUTO
workSteal tasklist = mapM async tasklist >>= mapM wait
#else
workSteal tasklist = do
  !tasksref <- newIORef tasklist
  !reslref <- newIORef []
  !finish <- newQSemN 0
  let pump = do
        !mbTask <- atomicModifyIORef tasksref (\tasks -> case tasks of (t:trest) -> (trest, Just t); _ -> ([], Nothing))
        case mbTask of
          Just t -> (do
                       !r <- t
                       atomicModifyIORef reslref (\rlist -> (r:rlist,()))
                    ) >> pump
          Nothing -> signalQSemN finish 1
  caps <- getNumCapabilities
  mapM_ (`forkOn` pump) [0 .. caps - 1]
  waitQSemN finish caps
  fmap reverse $ readIORef reslref
#endif

drive :: (EventLog -> IO [String]) -> IO ()
drive f = do
  putStrLn "Reading logs ..."
  -- Single threaded!
  !elogs <- mapM readEventLogFromFile =<< find always ((== ".eventlog") `liftM` extension) "."
  putStrLn "Converting logs ..."
  -- Multi threaded!
  !ct0 <- getCurrentTime
  let
    f1 (Left err) = putStrLn ("Failed with: "++ err) >> return []
    f1 (Right el) = f el
  !ss <- workSteal (map f1 elogs)
  ct1 <- getCurrentTime
  putStrLn $ "Conversion have taken " ++ show (diffUTCTime ct1 ct0) ++ " dumping ..."
  mapM_ (mapM_ putStrLn) ss
