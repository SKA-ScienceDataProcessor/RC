module Main where

import System.Directory
import System.Environment

import OskarBinReader
import Binner

main :: IO ()
main = do
  fn <- fmap head getArgs
  td <- readOskarData fn
  print "Task data are read."
  let prefix = "bins/"
  createDirectoryIfMissing True prefix
  bin prefix False td
  -- writeTaskData "" td
  finalizeTaskData td
