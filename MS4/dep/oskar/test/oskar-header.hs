
module Main where

import Data.Complex
import Control.Monad
import Foreign.Storable
import Foreign.C.Types
import System.Environment
import Text.Printf

import OskarReader

main :: IO ()
main = do
  args <- getArgs
  if length args < 1 then putStrLn "Usage: oskar-header [oskar file]" else do
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
    let showVis = "-vis" `elem` args
    forM_ [0..tdBaselines taskData-1] $ \bl -> do
      mm <- peekElemOff (tdBlMaxMin taskData) bl
      putStrLn $ "    BL " ++ show bl ++ ": " ++ show (mmMinW mm) ++ " - " ++ show (mmMaxW mm)
      when showVis $ do
        forM_ [0..tdTimes taskData-1] $ \t ->
            forM_ [0..tdChannels taskData-1] $ \ch -> do
                CDouble u <- peek $ tdUVWPtr taskData bl t 0
                CDouble v <- peek $ tdUVWPtr taskData bl t 1
                CDouble w <- peek $ tdUVWPtr taskData bl t 2
                putStr $ printf " %9.02f / %9.02f / %9.02f [Ch %2d]:" u v w ch
                forM_ [0..3] $ \p -> do
                   vr :+ vi <- peek $ tdVisibilityPtr taskData bl t ch p
                   putStr $ printf " %6.03f%-+.03fi" vr vi
                putStrLn ""
