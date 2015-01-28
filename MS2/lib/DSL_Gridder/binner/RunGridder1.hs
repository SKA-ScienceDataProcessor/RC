{-# LANGUAGE
      CPP
    , ScopedTypeVariables
  #-}

module Main where

import qualified Foreign.CUDA.Driver as CUDA
import System.IO.MMap (
    mmapFilePtr
  , munmapFilePtr
  , Mode(..)
  )
import Data.Maybe (catMaybes)
import Text.Printf (printf)

import System.Directory(doesFileExist)
import Foreign (Word8, plusPtr)

#define vis_size 64
#define vis_data_size 80

type RawPtr = CUDA.DevicePtr Word8

initialize :: IO (CUDA.Context, CUDA.Fun)
initialize = do
  CUDA.initialise []
  dev0 <- CUDA.device 0
  ctx <- CUDA.create dev0 [CUDA.SchedAuto]
  m <- CUDA.loadFile "grid_kernel.cubin"
  f <- CUDA.getFun m "grid_kernel"
  return (ctx, f)

main :: IO ()
main = let gridsize = 2048 * 2048 * vis_size in do
    (ctx, grid_kernel) <- initialize
    (grid_ptr_host, grid_rawsize, grid_offset, grid_size) <- mmapFilePtr "GPUBinned.dat" ReadWriteEx $ Just (0, gridsize)
    (gcf_ptr_host, gcf_rawsize, gcf_offset, gcf_size) <- mmapFilePtr "GCF.dat" ReadOnly Nothing
    CUDA.allocaArray gridsize $ \(grid_out :: RawPtr) -> do
      CUDA.memset grid_out gridsize 0
      CUDA.allocaArray gcf_size $ \(gcf_in :: RawPtr) -> do
        CUDA.pokeArray gcf_size (plusPtr gcf_ptr_host gcf_offset) gcf_in
        --
        let processBin (up, vp) = let binfile = printf "bins/000000-%03d-%03d" up vp in do
              doMe <- doesFileExist binfile
              if doMe
                then do
                  (data_ptr_host, data_rawsize, data_offset, data_size) <- mmapFilePtr binfile ReadOnly Nothing
                  data_in <- CUDA.mallocArray data_size :: IO RawPtr
                  CUDA.pokeArray data_size (plusPtr data_ptr_host data_offset) data_in
                  CUDA.launchKernel grid_kernel (16,16, 1) (8,8,1) 0 Nothing
                    [CUDA.IArg up, CUDA.IArg vp, CUDA.VArg data_in, CUDA.IArg(fromIntegral $ data_size `div` vis_data_size), CUDA.VArg gcf_in, CUDA.VArg grid_out]
                  munmapFilePtr data_ptr_host data_rawsize
                  return $ Just data_in
                else return Nothing
        resourcesToFree <- mapM processBin [(up, vp) | up <- [0..15], vp <- [0..15]]

        CUDA.peekArray grid_size grid_out (plusPtr grid_ptr_host grid_offset)
        munmapFilePtr grid_ptr_host grid_rawsize
        munmapFilePtr gcf_ptr_host gcf_rawsize
        -- Cleanup
        mapM_ (CUDA.free) (catMaybes resourcesToFree)
    CUDA.destroy ctx
