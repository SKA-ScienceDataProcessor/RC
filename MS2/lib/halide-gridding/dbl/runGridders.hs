{-# LANGUAGE BangPatterns #-}

module Main where

import System.IO (
    openBinaryFile
  , hClose
  , IOMode(..)
  , hPutBuf
  )
import Control.Exception (
    bracket
  , finally
  )
import System.IO.MMap (
    Mode(..)
  , mmapFilePtr
  , munmapFilePtr
  )
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (sizeOf)
-- import System.Environment (getArgs)
-- import System.FilePath (addExtension)
import Foreign.C.Types (CDouble)

import GriddersFFI

writeBinFile :: FilePath -> Ptr a -> Int -> IO ()
writeBinFile f p n = bracket (openBinaryFile f WriteMode) hClose
    (\h -> hPutBuf h p n)

shout :: String -> IO ()
shout = putStrLn . ("<<<< " ++) . (++ " >>>>")

-- ts = 10, blocks = 36, stations = 30, channels = 1: 10*36*30*(30-1)/2
runGridders
    :: FilePath
    -> FilePath
    -> IO ()
runGridders path_uvw path_amp = do
    (ptru, rawsizeu, offsetu, sizeu) <- mmapFilePtr path_uvw ReadOnly Nothing
    (ptra, rawsizea, offseta, sizea) <- mmapFilePtr path_amp ReadOnly Nothing
    if sizeu /= ts_x_bl_bytes * 3 || sizea /=  ts_x_bl_bytes * 8
      then shout "Wrong input dimensions"
      else
        finally
          (runG (ptru `plusPtr` offsetu) (ptra `plusPtr` offseta))
          (munmapFilePtr ptru rawsizeu >> munmapFilePtr ptra rawsizea)
  where
    runG uvwp ampp = do
      shout "Start Romein gridding"
      rr <- romeinComputeGridOnCuda uvwp ampp
      case rr of
        Left st -> shout $ "Romein finished with error: " ++ show st
        Right (GD datap finalizeRomein) -> do
          shout "Romein successfully finished, writing results ..."
          writeBinFile "romein_grid" datap grid_size
          shout "Romein data are written ..."
          finalizeRomein
          shout "Romein is done."
      --
      shout "Start Halide gridding"
      rh <- halideComputeGridOnCuda uvwp ampp
      case rh of
        Left st -> shout $ "Halide finished with error: " ++ show st
        Right (GD datap finalizeHalide) -> do
          shout "Halide successfully finished, writing results ..."
          writeBinFile "halide_grid" datap grid_size
          shout "Halide data are written ..."
          finalizeHalide
          shout "Halide is done."
    --
    grid_size = 2048 * 2048 * 8 * sizeOf (undefined :: CDouble)
    -- ts_per_block = 10, blocks = 36, stations = 30, channels = 1: 10*36*30*(30-1)/2 = 156600
    ts_x_bl = 10*36*15*29
    ts_x_bl_bytes = ts_x_bl * sizeOf (undefined :: CDouble)


main :: IO ()
main = runGridders "0-0.vis.uvw" "0-0.vis.amp"
