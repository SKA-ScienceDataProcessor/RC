{-# LANGUAGE
      CPP
    , TemplateHaskell
  #-}

module Main where

import Data.Int
import qualified CUDAEx as CUDA
import Foreign
import Foreign.C
-- import Data.Typeable
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString        as BS
import Data.Complex
import Foreign.Storable.Complex ()

import OskarBinReader
import GCF
import GPUGridder
import FFT

import DNA

binReaderActor :: Actor String TaskData
binReaderActor = actor (liftIO . readOskarData)

mkGcfCFG :: Bool -> CDouble -> GCFCfg
mkGcfCFG isFull (CDouble wstep) = GCFCfg {
    gcfcSuppDiv2Step = 4
  , gcfcLayersDiv2Plus1 = 32
  , gcfcIsFull = isFull
  , gcfcT2 = 0.2
  , gcfcWStep = wstep
  }

gcfCalcActor :: Actor GCFCfg GCF
gcfCalcActor = actor (liftIO . crGcf)
  where
    crGcf (GCFCfg hsupp_step n isFull t2 wstep) =
      let prep = if isFull then prepareFullGCF else prepareHalfGCF
      in createGCF t2 $ prep n hsupp_step wstep

-- Romein make the single kernel launch for all baselines with max support
simpleRomeinIter :: AddBaselinesIter
simpleRomeinIter baselines _mapper dev_mapper launch = launch ((32-1)*8+1) baselines dev_mapper

-- TODO: factor out host-to-GPU marshalling actor?
mkGPUGridderActor :: GridderConfig -> Actor (TaskData, GCF) Grid
mkGPUGridderActor gcfg = actor (liftIO . uncurry gridder)
  where gridder = runGridder gcfg

#define str(x) "x"
#define __SIMPLE_ROMEIN(perm, gcf, ishalf)                 \
simpleRomein/**/perm/**/gcf :: Actor (TaskData, GCF) Grid; \
simpleRomein/**/perm/**/gcf = mkGPUGridderActor (GridderConfig str(addBaselinesToGridSkaMid/**/perm/**/gcf) ishalf simpleRomeinIter)

__SIMPLE_ROMEIN(,FullGCF,True)
__SIMPLE_ROMEIN(,HalfGCF,False)
__SIMPLE_ROMEIN(UsingPermutations,FullGCF,True)
__SIMPLE_ROMEIN(UsingPermutations,HalfGCF,False)

-- Target array ('polp') must be preallocated
extractPolarizationActor :: Actor (Int32, CUDA.CxDoubleDevPtr, Grid) ()
extractPolarizationActor = actor (liftIO . go)
  where go (pol, polp, grid) = normalizeAndExtractPolarization pol polp grid

fftPolarizationActor :: Actor CUDA.CxDoubleDevPtr ()
fftPolarizationActor = actor (liftIO . fftGridPolarization)

gpuToHostActor :: Actor (Int, CUDA.CxDoubleDevPtr) CUDA.CxDoubleHostPtr
gpuToHostActor = actor (liftIO . go)
  where
    go (size, devptr) = do
      hostptr <- CUDA.mallocHostArray [] size 
      CUDA.peekArrayAsync size devptr hostptr Nothing
      CUDA.sync
      return hostptr

hostToDiskActor :: Actor (Int, CUDA.CxDoubleHostPtr, String) ()
hostToDiskActor = actor (liftIO . go)
  where
     go (size, hostptr, fname) =
       BS.unsafePackCStringLen (castPtr (CUDA.useHostPtr hostptr), size * sizeOf (undefined :: Complex Double)) >>= BS.writeFile fname

remotable [
    'binReaderActor
  , 'gcfCalcActor
  , 'simpleRomeinFullGCF
  , 'simpleRomeinHalfGCF
  , 'simpleRomeinUsingPermutationsFullGCF
  , 'simpleRomeinUsingPermutationsHalfGCF
  , 'extractPolarizationActor
  , 'fftPolarizationActor
  , 'gpuToHostActor
  , 'hostToDiskActor
  ]

-- Simple sequential 1-node program to test if
--   all parts work together.
main :: IO ()
main = dnaRun id $ do
    {-
      resBR <- select Local (N 0)
      shellBR <- startActor resBR $(mkStaticClosure 'binReaderActor)
      sendParam "test_p00_s00_f00.vis" shellBR
      futBR <- delay Local shellBR
      taskData <- await futBR
    -}
    taskData <- eval binReaderActor "test_p00_s00_f00.vis"
    gcf <- eval gcfCalcActor $ mkGcfCFG True (tdWstep taskData)
    grid <- eval simpleRomeinFullGCF (taskData, gcf)
    liftIO $ finalizeTaskData taskData
    liftIO $ finalizeGCF gcf
    polptr <- liftIO $ CUDA.mallocArray gridsize
    let
      extract n = do
        eval extractPolarizationActor (n, polptr, grid)
        eval fftPolarizationActor polptr
        hostpol <- eval gpuToHostActor (gridsize, polptr)
        eval hostToDiskActor (gridsize, hostpol, "test_p00_s00_f00_p" ++ show n)
        liftIO $ CUDA.freeHost hostpol
    extract 0
    extract 1
    extract 2
    extract 3
    liftIO $ finalizeGrid grid
    liftIO $ CUDA.free polptr
  where
    gridsize = 4096 * 4096
