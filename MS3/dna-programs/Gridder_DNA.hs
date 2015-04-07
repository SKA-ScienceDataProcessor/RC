{-# LANGUAGE
      CPP
    , ScopedTypeVariables
    , FlexibleInstances
    , TypeSynonymInstances
  #-}

module Main where

import Data.Int
import qualified CUDAEx as CUDA
import Foreign
import Data.Typeable
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

gcfCalcActor :: Actor GCFCfg GCF
gcfCalcActor = actor (liftIO . crGcf)
  where
    crGcf (GCFCfg hsupp_step n isFull t2 wstep) =
      let prep = if isFull then prepareFullGCF else prepareHalfGCF
      in createGCF t2 $ prep n hsupp_step wstep

-- TODO: factor out host-to-GPU marshalling actor?
mkGPUGridderActor :: GridderConfig -> Actor (TaskData, GCF) Grid
mkGPUGridderActor gcfg = actor (liftIO . uncurry gridder)
  where gridder = runGridder gcfg

-- Target array ('polp') must be preallocated
extractPolarizationActor :: Actor (Int32, CUDA.CxDoubleDevPtr, Grid) ()
extractPolarizationActor = actor (liftIO . go)
  where go (pol, polp, grid) = normalizeAndExtractPolarization pol polp grid

gpuToHostActor :: (Storable a, Typeable a) => Actor (Int, CUDA.DevicePtr a) (CUDA.HostPtr a)
gpuToHostActor = actor (liftIO . go)
  where
    go (size, devptr) = do
      hostptr <- CUDA.mallocHostArray [] size 
      CUDA.peekArrayAsync size devptr hostptr Nothing
      CUDA.sync
      return hostptr

hostToDiskActor :: forall a . (Storable a, Typeable a) => Actor (Int, CUDA.HostPtr a, String) ()
hostToDiskActor = actor (liftIO . go)
  where
     go (size, hostptr, fname) =
       BS.unsafePackCStringLen (castPtr (CUDA.useHostPtr hostptr), size * sizeOf (undefined :: a)) >>= BS.writeFile fname

-- Stub at the moment
main :: IO ()
main = return ()
