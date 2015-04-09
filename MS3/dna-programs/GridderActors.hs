{-# LANGUAGE
      CPP
    , TemplateHaskell
  #-}

module GridderActors where

import Data.Int
import qualified CUDAEx as CUDA
import Foreign
import Foreign.C
-- import Data.Typeable
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString        as BS
import Data.Complex
import Foreign.Storable.Complex ()
import System.FilePath

import Control.Distributed.Static (Closure)

import OskarBinReader
import Binner
import GCF
import GPUGridder
import FFT

import DNA

binReaderActor :: Actor String TaskData
binReaderActor = actor $
  profile "OskarBinaryReader" [ioHint{hintReadBytes = 565536297}] . liftIO . readOskarData

mkGcfCFG :: Bool -> CDouble -> GCFCfg
mkGcfCFG isFull (CDouble wstep) = GCFCfg {
    gcfcSuppDiv2Step = 4
  , gcfcLayersDiv2Plus1 = 32
  , gcfcIsFull = isFull
  , gcfcT2 = 0.2
  , gcfcWStep = wstep
  }

gcfCalcActor :: Actor GCFCfg GCFDev
gcfCalcActor = actor $ duration "GCF" . liftIO . crGcf
  where
    crGcf (GCFCfg hsupp_step n isFull t2 wstep) =
      let prep = if isFull then prepareFullGCF else prepareHalfGCF
      in createGCF t2 $ prep n hsupp_step wstep

-- Romein make the single kernel launch for all baselines with max support
simpleRomeinIter :: AddBaselinesIter
simpleRomeinIter baselines _mapper dev_mapper launch = launch ((32-1)*8+1) baselines dev_mapper

-- TODO: factor out host-to-GPU marshalling actor?
mkGPUGridderActor :: GridderConfig -> Actor (TaskData, GCFDev) Grid
mkGPUGridderActor gcfg = actor $ duration (gcKernelName gcfg) . liftIO . uncurry gridder
  where gridder = runGridder gcfg

#define str(x) "x"
#define __SIMPLE_ROMEIN(perm, gcf, isfull)                 \
simpleRomein/**/perm/**/gcf :: Actor (TaskData, GCFDev) Grid; \
simpleRomein/**/perm/**/gcf = mkGPUGridderActor (GridderConfig str(addBaselinesToGridSkaMid/**/perm/**/gcf) isfull simpleRomeinIter)

__SIMPLE_ROMEIN(,FullGCF,True)
__SIMPLE_ROMEIN(,HalfGCF,False)
__SIMPLE_ROMEIN(UsingPermutations,FullGCF,True)
__SIMPLE_ROMEIN(UsingPermutations,HalfGCF,False)

simpleRomeinUsingHalfOfFullGCF :: Actor (TaskData, GCFDev) Grid
simpleRomeinUsingHalfOfFullGCF = mkGPUGridderActor (GridderConfig "addBaselinesToGridSkaMidHalfGCF" True simpleRomeinIter)

-- Target array ('polp') must be preallocated
extractPolarizationActor :: Actor (Int32, CUDA.CxDoubleDevPtr, Grid) ()
extractPolarizationActor = actor $ duration "ExtractPolarizaton" . liftIO . go
  where go (pol, polp, grid) = normalizeAndExtractPolarization pol polp grid

fftPolarizationActor :: Actor CUDA.CxDoubleDevPtr ()
fftPolarizationActor = actor $ duration "FftPolarizaton" . liftIO . fftGridPolarization

gpuToHostActor :: Actor (Int, CUDA.CxDoubleDevPtr) CUDA.CxDoubleHostPtr
gpuToHostActor = actor go
  where
    go (size, devptr) = profile "GPU2Host" [cudaHint{hintCopyBytesHost = size * cdSize}] $ liftIO $ do
      hostptr <- CUDA.mallocHostArray [] size 
      CUDA.peekArrayAsync size devptr hostptr Nothing
      CUDA.sync
      return hostptr
    cdSize = sizeOf (undefined :: Complex Double)

hostToDiskActor :: Actor (Int, CUDA.CxDoubleHostPtr, String) ()
hostToDiskActor = actor go
  where
    go (size, hostptr, fname) = profile "Host2Disk" [ioHint{hintWriteBytes = size * cdSize}] $ liftIO $
      BS.unsafePackCStringLen (castPtr (CUDA.useHostPtr hostptr), size * sizeOf (undefined :: Complex Double)) >>= BS.writeFile fname
    cdSize = sizeOf (undefined :: Complex Double)

writeTaskDataActor :: Actor (String, TaskData) ()
writeTaskDataActor = actor (profile "WriteTaskData" [ioHint{hintReadBytes = 565762696}] . liftIO . uncurry writeTaskData)

readTaskDataActor :: Actor String TaskData
readTaskDataActor = actor (profile "ReadTaskData" [ioHint{hintReadBytes = 565762696}] . liftIO . readTaskData)

binAndPregridActor :: Actor (String, Bool, TaskData) ()
binAndPregridActor = actor (liftIO . go)
  where go (namespace, isForHalfGCF, td) = bin namespace isForHalfGCF td

mkGatherGridderActor :: GridderConfig -> Actor (String, TaskData, GCFDev) Grid
mkGatherGridderActor gcfg = actor (liftIO . gridder)
  where gridder (namespace, td, gcf) = runGatherGridder gcfg namespace td gcf

i0 :: AddBaselinesIter
i0 _ _ _ _ = return ()

gatherGridderActorFullGcf, gatherGridderActorHalfGcf :: Actor (String, TaskData, GCFDev) Grid
gatherGridderActorFullGcf = mkGatherGridderActor (GridderConfig "gridKernelGatherFullGCF" True i0)
gatherGridderActorHalfGcf = mkGatherGridderActor (GridderConfig "gridKernelGatherHalfGCF" False i0)

runGridderWith :: Closure (Actor (TaskData, GCFDev) Grid) -> TaskData -> String -> DNA ()
runGridderWith gridactor taskData ns_out = do
    gcf <- eval gcfCalcActor $ mkGcfCFG True (tdWstep taskData)
    grid <- evalClosure gridactor (taskData, gcf)
    liftIO $ finalizeTaskData taskData
    liftIO $ finalizeGCF gcf
    polptr <- liftIO $ CUDA.mallocArray gridsize
    let
      extract n = do
        eval extractPolarizationActor (n, polptr, grid)
        eval fftPolarizationActor polptr
        hostpol <- eval gpuToHostActor (gridsize, polptr)
        eval hostToDiskActor (gridsize, hostpol, ns_out </> 'p': show n)
        liftIO $ CUDA.freeHost hostpol
    extract 0
    extract 1
    extract 2
    extract 3
    liftIO $ finalizeGrid grid
    liftIO $ CUDA.free polptr
  where
    gridsize = 4096 * 4096

runGridderOnSavedData :: Actor(String, String, Closure (Actor (TaskData, GCFDev) Grid)) ()
runGridderOnSavedData = actor $ \(ns_in, ns_out, gridactor) -> do
  taskData <- eval readTaskDataActor ns_in
  runGridderWith gridactor taskData ns_out

runGridderOnLocalData :: Actor(String, TaskData, Closure (Actor (TaskData, GCFDev) Grid)) ()
runGridderOnLocalData = actor $ \(ns_out, taskdata, gridactor) ->
  runGridderWith gridactor taskdata ns_out

remotable [
    'binReaderActor
  , 'writeTaskDataActor
  , 'readTaskDataActor
  , 'binAndPregridActor
  , 'gatherGridderActorFullGcf
  , 'gatherGridderActorHalfGcf
  , 'gcfCalcActor
  , 'simpleRomeinFullGCF
  , 'simpleRomeinHalfGCF
  , 'simpleRomeinUsingPermutationsFullGCF
  , 'simpleRomeinUsingPermutationsHalfGCF
  , 'simpleRomeinUsingHalfOfFullGCF
  , 'extractPolarizationActor
  , 'fftPolarizationActor
  , 'gpuToHostActor
  , 'hostToDiskActor
  , 'runGridderOnSavedData
  , 'runGridderOnLocalData
  ]
