
module Kernel.GPU.ScatterGrid
  ( prepare
  , createGrid
  , grid
  , degrid
  ) where

import Control.Monad
import Data.Word
import Foreign.Ptr
import Foreign.Storable.Complex ()

import Data
import Kernel.GPU.Common as CUDA
import Vector

pregriddedSize :: Int
pregriddedSize = shortSize * 5 + ptrSize
  where shortSize = 16
        ptrSize = 64

foreign import ccall unsafe "&" scatter_grid_phaseRotate :: CUDA.Fun
foreign import ccall unsafe "&" scatter_grid_preparePos :: CUDA.Fun
foreign import ccall unsafe "&" scatter_grid :: CUDA.Fun

{-
launchAddBaselines :: TaskData -> CUDA.Fun -> AddBaselinesFun
launchAddBaselines td f scale gridptr gcflp uvwp visp permp maxSupp blOff numOfBaselines =
  CUDA.launchKernel f (numOfBaselines, 1, 1) (nOfThreads, 1, 1) 0 Nothing params
  where
    nOfThreads = min 1024 (maxSupp * maxSupp)
    params = mapArgs
          scale
          (tdWstep td)
          permp
          gridptr
          gcflp
          uvwp
          visp
          (fromIntegral blOff :: Int32)
-}

prepare :: GridPar -> Vis -> GCFSet -> IO (Vis, GCFSet)
prepare gridp vis gcfSet = do

  -- Transfer all GCFs to GPU, generate gcf pointer list
  let gcfp = gcfsPar gcfSet
  gcfMap <- allocHostVector $ length $ gcfs gcfSet :: IO (Vector (Ptr ()))
  gcfs' <- forM (gcfs gcfSet) $ \gcf -> do
    data' <- toHostVector (gcfData gcf)
    -- TODO
    return gcf{gcfData = data'}

  -- Transfer visibilities and positions, do phase rotation (TODO:
  -- This should probably be a separate top-level step...)
  visData' <- toDeviceVector (visData vis)
  visPos' <- toDeviceVector (visPositions vis)
  let visibilities = vectorSize $ visData vis
  rotatedVis <- allocDeviceVector visibilities
  CUDA.launchKernel scatter_grid_phaseRotate
    (1024,1,1) (1,1,1) 0 Nothing $
    mapArgs visData' visPos' rotatedVis visibilities

  -- Pregrid parameters
  let scale = fromIntegral (gridWidth gridp) / gridLambda gridp
      stepw = gcfpStepW $ gcfsPar gcfSet

  -- Pregrid baselines
  let totalPregridSize = pregriddedSize * visibilities
  pregridded <- allocDeviceVector totalPregridSize :: IO (Vector Word8)
  forM_ (visBaselines vis) $ \bl -> do

    let minGCF = maybe (gcfpMinSize gcfp) gcfSize $ findGCF gcfSet (vblMinW bl)
        maxGCF = maybe (gcfpMinSize gcfp) gcfSize $ findGCF gcfSet (vblMaxW bl)
        max_supp = min minGCF maxGCF

    CUDA.launchKernel scatter_grid_preparePos
      (1024,1,1) (1,1,1) 0 Nothing $
      mapArgs scale stepw max_supp visData' visPos' rotatedVis visibilities

  return (vis{ visData = rotatedVis },
          gcfSet{ gcfs = gcfs' })

createGrid :: GridPar -> GCFPar -> IO UVGrid
createGrid gp gcfp = do
   dat <- allocDeviceVector (gridFullSize gp)
   return $ UVGrid gp 0 dat

grid :: Vis -> GCFSet -> UVGrid -> IO UVGrid
grid = undefined

degrid :: UVGrid -> GCFSet -> Vis -> IO Vis
degrid = undefined

