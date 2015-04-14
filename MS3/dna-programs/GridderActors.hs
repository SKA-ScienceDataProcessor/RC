{-# LANGUAGE
      CPP
    , TemplateHaskell
  #-}

module GridderActors where

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

import qualified CPUGridder as CPU

import DNA

binReader :: String -> DNA TaskData
binReader = profile "OskarBinaryReader" [ioHint{hintReadBytes = 565536297}] . liftIO . readOskarData

mkGcfCFG :: Bool -> CDouble -> GCFCfg
mkGcfCFG isFull (CDouble wstep) = GCFCfg {
    gcfcSuppDiv2Step = 4
  , gcfcLayersDiv2Plus1 = 32
  , gcfcIsFull = isFull
  , gcfcT2 = 0.2
  , gcfcWStep = wstep
  }

gcfCalc :: GCFCfg -> DNA GCFDev
gcfCalc = duration "GCF" . liftIO . crGcf
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

cdSize :: Int
cdSize = sizeOf (undefined :: Complex Double)
{-# INLINE cdSize #-}

gpuToHost :: Int -> CUDA.CxDoubleDevPtr -> DNA CUDA.CxDoubleHostPtr
gpuToHost size devptr = profile "GPU2Host" [cudaHint{hintCopyBytesHost = size * cdSize}] $ liftIO $ do
    hostptr <- CUDA.mallocHostArray [] size 
    CUDA.peekArrayAsync size devptr hostptr Nothing
    CUDA.sync
    return hostptr

hostToDisk :: Int -> Ptr (Complex Double) -> String -> DNA ()
hostToDisk size hostptr fname = profile "Host2Disk" [ioHint{hintWriteBytes = size * cdSize}] $ liftIO $
    BS.unsafePackCStringLen (castPtr hostptr, size * cdSize) >>= BS.writeFile fname

writeTaskDataP :: String -> TaskData -> DNA ()
writeTaskDataP = (profile "WriteTaskData" [ioHint{hintWriteBytes = 565762696}] .) . (liftIO .) . writeTaskData

readTaskDataP :: String -> DNA TaskData
readTaskDataP = profile "ReadTaskData" [ioHint{hintReadBytes = 565762696}] . liftIO . readTaskData

binAndPregrid :: String -> Bool -> TaskData -> DNA ()
binAndPregrid namespace isForHalfGCF td = duration "Binner" . liftIO $
  bin namespace isForHalfGCF td

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
    gcf <- gcfCalc $ mkGcfCFG True (tdWstep taskData)
    grid <- evalClosure gridactor (ns_in, taskData, gcf)
    liftIO $ finalizeGCF gcf
    polptr <- liftIO $ CUDA.mallocArray gridsize
    let
      extract n = do
        duration "ExtractPolarizaton" . liftIO $ normalizeAndExtractPolarization n polptr grid
        duration "FftPolarizaton" . liftIO $ fftGridPolarization polptr
        hostpol <- gpuToHost gridsize polptr
        hostToDisk gridsize (CUDA.useHostPtr hostpol) (ns_out </> 'p': show n)
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
  taskData <- readTaskDataP ns_in
  runGridderWith gridactor taskData "" ns_out
  liftIO $ finalizeTaskData taskData

runGridderOnLocalData :: Actor(String, TaskData, Closure (Actor (String, TaskData, GCFDev) Grid)) ()
runGridderOnLocalData = actor $ \(ns_out, taskdata, gridactor) ->
  runGridderWith gridactor taskdata "" ns_out

marshalGCF2HostP :: GCFDev -> DNA GCFHost
marshalGCF2HostP gcfd@(GCF gcfsize nol _ _) =
    profile "MarshalGCF2Host" [cudaHint{hintCopyBytesHost = gcfsize * cdSize + nol * 8 * 8 * pSize}]
      $ liftIO $ marshalGCF2Host gcfd
  where
    pSize = sizeOf (undefined :: CUDA.DevicePtr (CUDA.CxDoubleDevPtr))


-- Need 32-byte alignment for AVX m256-family. GHC has no them. Using
--  aligned alloca is possible only in IO. Thus need this.
-- foreign import ccall unsafe aligned_alloc :: Int -> Int -> IO (Ptr a)
foreign import ccall unsafe valloc :: Int -> IO (Ptr a)

-- We could import foreign functions directly (not their pointers),
-- but pointers can be passed to actors because they have binary instances
-- and while we don't do this at the moment we leave the door open to
-- this in future. 
#define __MKP(a) (CPU.mkCPUGridderFun CPU.a/**/_ptr, "a")

-- Use no GridderConfig here
-- We have 4 variants only and all are covered by this code
cpuGridder :: Bool -> Bool -> Bool -> String -> TaskData -> GCFHost -> DNA ()
cpuGridder isFullGcf useFullGcf usePermutations ns_out td gcfh = do
    gridp <- liftIO $ {-aligned_alloc 32-} valloc (gridsize * 4 * cdSize)
    duration gname $ liftIO $ gfun scale (tdWstep td) (tdMap td) gridp gcfp (tdUVWs td) (tdVisibilies td)
    polptr <- liftIO $ {-aligned_alloc 32-} valloc (gridsize * cdSize)
    let extract n = do
          duration "ExtractPolarizatonCPU" . liftIO $ CPU.normalizeAndExtractPolarization n polptr gridp
          duration "FftPolarizatonCPU" . liftIO $ CPU.fft_inplace_even polptr
          hostToDisk gridsize polptr (ns_out </> 'p': show n)
    extract 0
    extract 1
    extract 2
    extract 3
    liftIO $ free polptr
    liftIO $ free gridp
  where
    gcfp = if isFullGcf then getCentreOfFullGCFHost gcfh else gcfLayers gcfh
    gridsize = 4096 * 4096
    scale = let CDouble sc = (2048 - 124 - 1) / (tdMaxx td) in sc -- 124 max hsupp
    (gfun, gname) = mk useFullGcf usePermutations
    --
    mk True  False = __MKP(gridKernelCPUFullGCF)
    mk True  True  = __MKP(gridKernelCPUFullGCFPerm)
    mk False False = __MKP(gridKernelCPUHalfGCF)
    mk False True  = __MKP(gridKernelCPUHalfGCFPerm)


mkGcfAndCpuGridderActor :: Bool -> Bool -> Bool -> Actor(String, TaskData) ()
mkGcfAndCpuGridderActor isFullGcf useFullGcf usePermutations = actor go
  where
    go (ns_out, td) = do
      gcfd <- gcfCalc $ mkGcfCFG True (tdWstep td)
      gcfh <- marshalGCF2HostP gcfd
      liftIO $ finalizeGCF gcfd
      cpuGridder isFullGcf useFullGcf usePermutations ns_out td gcfh
      liftIO $ finalizeGCFHost gcfh

remotable [
    'gatherGridderActorFullGcf
  , 'gatherGridderActorHalfGcf
  , 'simpleRomeinFullGCF
  , 'simpleRomeinHalfGCF
  , 'simpleRomeinUsingPermutationsFullGCF
  , 'simpleRomeinUsingPermutationsHalfGCF
  , 'simpleRomeinUsingHalfOfFullGCF
  , 'runGridderOnSavedData
  , 'runGridderOnLocalData
  ]
