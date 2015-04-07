{-# LANGUAGE
      CPP
    , TemplateHaskell
  #-}

module Main where

import Data.Int
import qualified CUDAEx as CUDA
import Foreign
-- import Data.Typeable
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString        as BS

{-
import Data.Complex
import Foreign.Storable.Complex ()
 -}

import OskarBinReader
import GCF
import GPUGridder

import DNA

binReaderActor :: Actor String TaskData
binReaderActor = actor (liftIO . readOskarData)

mkGcfCFG :: Bool -> Double -> GCFCfg
mkGcfCFG isFull wstep = GCFCfg {
    gcfcSuppDiv2Step = 4
  , gcfcLayersDiv2 = 31
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
simpleRomeinIter baselines _mapper dev_mapper launch = launch 249 baselines dev_mapper

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

gpuToHostActor :: Actor (Int, CUDA.DevicePtr Double) (CUDA.HostPtr Double)
gpuToHostActor = actor (liftIO . go)
  where
    go (size, devptr) = do
      hostptr <- CUDA.mallocHostArray [] size 
      CUDA.peekArrayAsync size devptr hostptr Nothing
      CUDA.sync
      return hostptr

hostToDiskActor :: Actor (Int, CUDA.HostPtr Double, String) ()
hostToDiskActor = actor (liftIO . go)
  where
     go (size, hostptr, fname) =
       BS.unsafePackCStringLen (castPtr (CUDA.useHostPtr hostptr), size * sizeOf (undefined :: Double)) >>= BS.writeFile fname

remotable [
    'binReaderActor
  , 'gcfCalcActor
  , 'simpleRomeinFullGCF
  , 'simpleRomeinHalfGCF
  , 'simpleRomeinUsingPermutationsFullGCF
  , 'simpleRomeinUsingPermutationsHalfGCF
  , 'extractPolarizationActor
  , 'gpuToHostActor
  , 'hostToDiskActor
  ]

-- Stub at the moment
main :: IO ()
main = return ()
