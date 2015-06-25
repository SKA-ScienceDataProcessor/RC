
module Oskar where

import DNA.Channel.File

import OskarReader

import Config
import Data

-- | Just reads the header from an Oskar file, to determine how much
-- work they represent.
--
-- TODO: Currently still reads the whole file!
readOskarHeader :: FileChan OskarData -> String -> IO ([Polar], [Int])
readOskarHeader chan file = do
  taskData <- readOskarData (getFileChan chan file)
  finalizeTaskData taskData
  return ([XX,YY,XY,YX], [0..tdChannels taskData-1])
