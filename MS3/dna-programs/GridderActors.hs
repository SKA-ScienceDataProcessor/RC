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
import OskarBinReaderFFI
import Binner
import GCF
import GPUGridder
import FFT
import Utils

import qualified CPUGridder as CPU

import DNA

binReader :: String -> DNA TaskData
binReader = unboundKernel "OskarBinaryReader" [ioHint{hintReadBytes = 565536297}] . liftIO . readOskarData

cdSize :: Int
cdSize = sizeOf (undefined :: Complex Double)
{-# INLINE cdSize #-}

mkGcfCFG :: Bool -> CDouble -> GCFCfg
mkGcfCFG isfull (CDouble wstp) = GCFCfg {
    gcfcSuppDiv2Step = 4
  , gcfcLayersDiv2Plus1 = 30
  , gcfcIsFull = isfull
  , gcfcT2 = 0.2
  , gcfcWStep = wstp
  }

gcfCalc :: GCFCfg -> DNA GCFDev
gcfCalc (GCFCfg hsupp_step n isfull t2 wstp) =
    kernel "GCF" [ cudaHint{ hintCudaDoubleOps = flopsPerCD * ws_size
                            } ] $ liftIO $ createGCF isfull t2 ws_hsupps_size

  where
    ws_hsupps_size = prepareGCF isfull n 17 hsupp_step wstp
    ws_size = snd ws_hsupps_size
    flopsPerCD = 400 -- determined experimentally

-- Romein make the single kernel launch for all baselines with max support
simpleRomeinIter :: AddBaselinesIter
simpleRomeinIter baselines _mapper launch = launch ((30-1)*8+17) 0 baselines

-- Makes sense only for abs sorted mappers.
optRomeinIter :: AddBaselinesIter
optRomeinIter baselines _mapper launch = do
    mapp0 <- go 0 _mapper
    mapp1 <- go 1 mapp0
    let
      off0 = minusPtr mapp0 _mapper `div` msiz
      off1 = minusPtr mapp1 _mapper `div` msiz
    -- hardcoded max support sizes ATM
    launch 17    0              off0
    launch 25 off0 (     off1 - off0)
    launch 33 off1 (baselines - off1) -- any max_supp > 32 would go
  where
    msiz = sizeOf (undefined :: BlWMap)
    go n mapp = do
      wp <- wplane mapp
      if abs wp <= n
        then go n (advancePtr mapp 1)
        else return mapp

-- TODO: factor out host-to-GPU marshalling actor?
mkGPUGridderActor :: GridderConfig -> Actor (String, TaskData, GCFDev) Grid
mkGPUGridderActor gcfg = actor $
    kernel (gcKernelName gcfg)
      [ cudaHint{ hintCopyBytesDevice = 565762648
                , hintCudaDoubleOps = 322380900646
                }] . liftIO . gridder
  where gridder (_, td, gcf) = runGridder gcfg td gcf

#define str(x) "x"
#define __mkWith(mk, k,f,i) mk (GridderConfig str(k) k f i)

#define __SIMPLE_ROMEIN(perm, gcf, isfull)                                           \
foreign import ccall unsafe "&" addBaselinesToGridSkaMid/**/perm/**/gcf :: CUDA.Fun; \
simpleRomein/**/perm/**/gcf :: Actor (String, TaskData, GCFDev) Grid;                \
simpleRomein/**/perm/**/gcf = __mkWith(mkGPUGridderActor, addBaselinesToGridSkaMid/**/perm/**/gcf, isfull, simpleRomeinIter)

__SIMPLE_ROMEIN(,FullGCF,True)
__SIMPLE_ROMEIN(,HalfGCF,False)
__SIMPLE_ROMEIN(UsingPermutations,FullGCF,True)
__SIMPLE_ROMEIN(UsingPermutations,HalfGCF,False)

simpleRomeinUsingHalfOfFullGCF, optRomeinFullGCF :: Actor (String, TaskData, GCFDev) Grid
simpleRomeinUsingHalfOfFullGCF = __mkWith(mkGPUGridderActor, addBaselinesToGridSkaMidHalfGCF, True, simpleRomeinIter)
optRomeinFullGCF = __mkWith(mkGPUGridderActor, addBaselinesToGridSkaMidUsingPermutationsFullGCF, True, optRomeinIter)

gpuToHost :: Int -> CUDA.CxDoubleDevPtr -> DNA CUDA.CxDoubleHostPtr
gpuToHost size devptr = kernel "GPU2Host" [cudaHint{hintCopyBytesHost = size * cdSize}] $ liftIO $ do
    hostptr <- CUDA.mallocHostArray [] size 
    CUDA.peekArrayAsync size devptr hostptr Nothing
    CUDA.sync
    return hostptr

hostToDisk :: Int -> Ptr (Complex Double) -> String -> DNA ()
hostToDisk size hostptr fname = kernel "Host2Disk" [ioHint{hintWriteBytes = size * cdSize}] $ liftIO $
    BS.unsafePackCStringLen (castPtr hostptr, size * cdSize) >>= BS.writeFile fname

writeTaskDataP :: String -> TaskData -> DNA ()
writeTaskDataP = (kernel "WriteTaskData" [ioHint{hintWriteBytes = 565762696}] .) . (liftIO .) . writeTaskData

readTaskDataP :: String -> DNA TaskData
readTaskDataP = kernel "ReadTaskData" [ioHint{hintReadBytes = 565762696}] . liftIO . readTaskData

binAndPregrid :: String -> Bool -> TaskData -> DNA ()
binAndPregrid namespace isForHalfGCF td = kernel "Binner" [] . liftIO $
  bin namespace isForHalfGCF td

mkGatherGridderActor :: GridderConfig -> Actor (String, TaskData, GCFDev) Grid
mkGatherGridderActor gcfg =
  actor (kernel (gcKernelName gcfg)
           [ ioHint {hintReadBytes = 1850400000}
           , cudaHint {hintCopyBytesDevice = 1850400000}
           , cudaHint {hintCudaDoubleOps = 51360000000}
           ] . liftIO . gridder)
  where gridder (namespace, td, gcf) = runGatherGridder gcfg namespace td gcf

i0 :: AddBaselinesIter
i0 _ _ _ = return ()

#define __k(n) foreign import ccall unsafe "&" n :: CUDA.Fun
__k(gridKernelGatherFullGCF)
__k(gridKernelGatherHalfGCF)
gatherGridderActorFullGcf, gatherGridderActorHalfGcf :: Actor (String, TaskData, GCFDev) Grid
gatherGridderActorFullGcf = __mkWith(mkGatherGridderActor, gridKernelGatherFullGCF, True, i0)
gatherGridderActorHalfGcf = __mkWith(mkGatherGridderActor, gridKernelGatherHalfGCF, False, i0)

runGridderWith :: Closure (Actor (String, TaskData, GCFDev) Grid) -> TaskData -> String -> String -> DNA ()
runGridderWith gridactor taskData ns_in ns_out = do
    gcf <- gcfCalc $ mkGcfCFG True (tdWstep taskData)
    grid <- evalClosure gridactor (ns_in, taskData, gcf)
    polptr <- kernel "runGridderWith/mem" [] $ liftIO $ do
        finalizeGCF gcf
        CUDA.mallocArray gridsize
    let
      extract n = do
        kernel "ExtractPolarizaton" [] . liftIO $ normalizeAndExtractPolarization n polptr grid
        kernel "FftPolarizaton" [] . liftIO $ fftGridPolarization polptr
        hostpol <- gpuToHost gridsize polptr
        hostToDisk gridsize (CUDA.useHostPtr hostpol) (ns_out </> 'p': show n)
        kernel "runGridderWith/mem" [] $ liftIO $ CUDA.freeHost hostpol
    extract 0
    extract 1
    extract 2
    extract 3
    kernel "runGridderWith/mem" [] $ liftIO $ do
        finalizeGrid grid
        CUDA.free polptr
  where
    gridsize = 4096 * 4096

runGridderOnSavedData :: Actor(String, String, Closure (Actor (String, TaskData, GCFDev) Grid)) ()
runGridderOnSavedData = actor $ \(ns_in, ns_out, gridactor) -> do
  taskData <- readTaskDataP ns_in
  runGridderWith gridactor taskData "" ns_out
  kernel "runGridderOnSavedData/mem" [] $ liftIO $
    finalizeTaskData taskData

runGridderOnLocalData :: Actor(String, TaskData, Closure (Actor (String, TaskData, GCFDev) Grid)) ()
runGridderOnLocalData = actor $ \(ns_out, taskdata, gridactor) ->
  runGridderWith gridactor taskdata "" ns_out

marshalGCF2HostP :: GCFDev -> DNA GCFHost
marshalGCF2HostP gcfd@(GCF gcfsize nol _ _ _) =
    kernel "MarshalGCF2Host" [cudaHint{hintCopyBytesHost = gcfsize * cdSize + nol * 8 * 8 * pSize}]
      $ liftIO $ marshalGCF2Host gcfd
  where
    pSize = sizeOf (undefined :: CUDA.DevicePtr (CUDA.CxDoubleDevPtr))


-- We could import foreign functions directly (not their pointers),
-- but pointers can be passed to actors because they have binary instances
-- and while we don't do this at the moment we leave the door open to
-- this in future. 
#define __MKP(a) (CPU.mkCPUGridderFun CPU.a/**/_ptr, "a")

-- Use no GridderConfig here
-- We have 4 variants only and all are covered by this code
cpuGridder :: Bool -> Bool -> String -> TaskData -> GCFHost -> DNA ()
cpuGridder useFullGcf usePermutations ns_out td gcfh = do
    (gridp, polptr) <- kernel gname [floatHint {hintDoubleOps = 5211646626}] $ liftIO $ do
        gridp <- alignedMallocArray (gridsize * 4) 32
        gfun scale (tdWstep td) (tdMap td) gridp (getLayersHost gcfh) (tdUVWs td) (tdVisibilies td)
        polptr <- mallocArray gridsize
        return (gridp, polptr)
    let extract n = do
          kernel "ExtractPolarizatonCPU" [] . liftIO $ CPU.normalizeAndExtractPolarizationCPU n polptr gridp
          kernel "FftPolarizatonCPU" [] . liftIO $ CPU.fft_inplace_even polptr
          hostToDisk gridsize polptr (ns_out </> 'p': show n)
    extract 0
    extract 1
    extract 2
    extract 3
    kernel gname [] $ liftIO $ do
      free polptr
      free gridp
  where
    gridsize = 4096 * 4096
    scale = let CDouble sc = (2048 - 124 - 1) / (tdMaxx td) in sc -- 124 max hsupp
    (gfun, gname) = mk useFullGcf usePermutations
    --
    mk True  False = __MKP(gridKernelCPUFullGCF)
    mk True  True  = __MKP(gridKernelCPUFullGCFPerm)
    mk False False = __MKP(gridKernelCPUHalfGCF)
    mk False True  = __MKP(gridKernelCPUHalfGCFPerm)


mkGcfAndCpuGridder :: Bool -> Bool -> Bool -> String -> TaskData -> DNA ()
mkGcfAndCpuGridder isFullGcf useFullGcf usePermutations ns_out td = do
  gcfd <- gcfCalc $ mkGcfCFG isFullGcf (tdWstep td)
  gcfh <- marshalGCF2HostP gcfd
  kernel "mkGcfAndCpuGridder/mem" [] $ liftIO $
    finalizeGCF gcfd
  cpuGridder useFullGcf usePermutations ns_out td gcfh
  kernel "mkGcfAndCpuGridder/mem" [] $ liftIO $
    finalizeGCFHost gcfh

remotable [
    'gatherGridderActorFullGcf
  , 'gatherGridderActorHalfGcf
  , 'simpleRomeinFullGCF
  , 'simpleRomeinHalfGCF
  , 'simpleRomeinUsingPermutationsFullGCF
  , 'simpleRomeinUsingPermutationsHalfGCF
  , 'simpleRomeinUsingHalfOfFullGCF
  , 'optRomeinFullGCF
  , 'runGridderOnSavedData
  , 'runGridderOnLocalData
  ]
