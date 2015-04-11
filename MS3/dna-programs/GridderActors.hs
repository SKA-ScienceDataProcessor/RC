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

import CPUGridder

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
mkGPUGridderActor :: GridderConfig -> Actor (String, TaskData, GCFDev) Grid
mkGPUGridderActor gcfg = actor $ profile (gcKernelName gcfg) [cudaHint{hintCopyBytesDevice = 565762648}] . liftIO . gridder
  where gridder (_, td, gcf) = runGridder gcfg td gcf

#define str(x) "x"
#define __SIMPLE_ROMEIN(perm, gcf, isfull)                 \
simpleRomein/**/perm/**/gcf :: Actor (String, TaskData, GCFDev) Grid; \
simpleRomein/**/perm/**/gcf = mkGPUGridderActor (GridderConfig str(addBaselinesToGridSkaMid/**/perm/**/gcf) isfull simpleRomeinIter)

__SIMPLE_ROMEIN(,FullGCF,True)
__SIMPLE_ROMEIN(,HalfGCF,False)
__SIMPLE_ROMEIN(UsingPermutations,FullGCF,True)
__SIMPLE_ROMEIN(UsingPermutations,HalfGCF,False)

simpleRomeinUsingHalfOfFullGCF :: Actor (String, TaskData, GCFDev) Grid
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
writeTaskDataActor = actor (profile "WriteTaskData" [ioHint{hintWriteBytes = 565762696}] . liftIO . uncurry writeTaskData)

readTaskDataActor :: Actor String TaskData
readTaskDataActor = actor (profile "ReadTaskData" [ioHint{hintReadBytes = 565762696}] . liftIO . readTaskData)

binAndPregridActor :: Actor (String, Bool, TaskData) ()
binAndPregridActor = actor (duration "Binner" . liftIO . go)
  where go (namespace, isForHalfGCF, td) = bin namespace isForHalfGCF td

mkGatherGridderActor :: GridderConfig -> Actor (String, TaskData, GCFDev) Grid
mkGatherGridderActor gcfg = actor (duration (gcKernelName gcfg) . liftIO . gridder)
  where gridder (namespace, td, gcf) = runGatherGridder gcfg namespace td gcf

i0 :: AddBaselinesIter
i0 _ _ _ _ = return ()

gatherGridderActorFullGcf, gatherGridderActorHalfGcf :: Actor (String, TaskData, GCFDev) Grid
gatherGridderActorFullGcf = mkGatherGridderActor (GridderConfig "gridKernelGatherFullGCF" True i0)
gatherGridderActorHalfGcf = mkGatherGridderActor (GridderConfig "gridKernelGatherHalfGCF" False i0)

runGridderWith :: Closure (Actor (String, TaskData, GCFDev) Grid) -> TaskData -> String -> String -> DNA ()
runGridderWith gridactor taskData ns_in ns_out = do
    gcf <- eval gcfCalcActor $ mkGcfCFG True (tdWstep taskData)
    grid <- evalClosure gridactor (ns_in, taskData, gcf)
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

runGridderOnSavedData :: Actor(String, String, Closure (Actor (String, TaskData, GCFDev) Grid)) ()
runGridderOnSavedData = actor $ \(ns_in, ns_out, gridactor) -> do
  taskData <- eval readTaskDataActor ns_in
  runGridderWith gridactor taskData "" ns_out

runGridderOnLocalData :: Actor(String, TaskData, Closure (Actor (String, TaskData, GCFDev) Grid)) ()
runGridderOnLocalData = actor $ \(ns_out, taskdata, gridactor) ->
  runGridderWith gridactor taskdata "" ns_out

marshalGCF2HostActor :: Actor GCFDev GCFHost
marshalGCF2HostActor = actor go
  where
    go gcfd@(GCF gcfsize nol _ _) =
      profile "MarshalGCF2Host" [cudaHint{hintCopyBytesHost = gcfsize * cdSize + nol * 8 * 8 * pSize}]
        $ liftIO $ marshalGCF2Host gcfd
    cdSize = sizeOf (undefined :: Complex Double)
    pSize = sizeOf (undefined :: CUDA.DevicePtr (CUDA.CxDoubleDevPtr))


-- We could import foreign functions directly (not their pointers),
-- but pointers can be passed to actors because they have binary instances
-- and while we don't do this at the moment we leave the door open to
-- this in future. 
#define __MKP(a) (mkCPUGridderFun a/**/_ptr, "a")

-- Use no GridderConfig here
-- We have 4 variants only and all are covered by this code
mkCpuGridderActor :: Bool -> Bool -> Actor(Double, TaskData, GCFHost) (Ptr (Complex Double))
mkCpuGridderActor isFullGcf usePermutations = actor go
  where
    go (scale, td, gcfh) = duration gname $ liftIO $ do
      let gcfp = if isFullGcf then getCentreOfFullGCFHost gcfh else gcfLayers gcfh
      gridp <- mallocArray gridsize
      gfun scale (tdMap td) gridp gcfp (tdUVWs td) (tdVisibilies td)
      return gridp
    gridsize = 4096 * 4096 * 4
    (gfun, gname) = mk isFullGcf usePermutations
    mk True  False = __MKP(gridKernelCPUFullGCF)
    mk True  True  = __MKP(gridKernelCPUFullGCFPerm)
    mk False False = __MKP(gridKernelCPUHalfGCF)
    mk False True  = __MKP(gridKernelCPUHalfGCFPerm)


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
  , 'mkCpuGridderActor
  ]
