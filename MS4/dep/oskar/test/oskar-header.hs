
module Main where

import Control.Monad
import Foreign.Storable
import System.Environment

import OskarReader

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then putStrLn "Usage: oskar-header [oskar file]" else do
    taskData <- readOskarData (head args)
    putStrLn $ unlines
      [ "Oskar header:"
      , " Baselines:          " ++ show (tdBaselines taskData)
      , " Timesteps:          " ++ show (tdTimes taskData)
      , " Frequency Channels: " ++ show (tdChannels taskData)
      , " Points:             " ++ show (tdPoints taskData)
      , " U range:            " ++ show (mtxMinU $ tdMetrix taskData) ++ " - "
                                ++ show (mtxMaxU $ tdMetrix taskData)
      , " V range:            " ++ show (mtxMinV $ tdMetrix taskData) ++ " - "
                                ++ show (mtxMaxV $ tdMetrix taskData)
      , " W range:            " ++ show (mtxMinW $ tdMetrix taskData) ++ " - "
                                ++ show (mtxMaxW $ tdMetrix taskData)
      , " Baselines W ranges:"
      ]
    forM_ [0..tdBaselines taskData-1] $ \i -> do
      mm <- peekElemOff (tdBlMaxMin taskData) i
      putStrLn $ "    BL " ++ show i ++ ": " ++ show (mmMinW mm) ++ " - " ++ show (mmMaxW mm) 

