{-# LANGUAGE
      CPP
    , ScopedTypeVariables
    , FlexibleInstances
    , TypeSynonymInstances
    , DeriveGeneric
    , DeriveDataTypeable
    , BangPatterns
  #-}

module Kernel.GPU.GCF ( kernel ) where

import Control.Monad

import Foreign.Storable
import Foreign.Storable.Complex ()
import Foreign.C

import Data
import Vector

import qualified Kernel.GPU.Common as CUDA
import Kernel.GPU.Common (CxDoubleDevPtr, CxDouble)
import Kernel.GPU.FFT


#define __k(n) foreign import ccall unsafe "&" n :: CUDA.Fun

__k(reduce_512_e2)
__k(r2)
__k(wkernff)
__k(copy_2_over)
__k(transpose_over0)
__k(normalize)
__k(wextract1)

launchOnFF :: CUDA.Fun -> Int -> [CUDA.FunParam] -> IO ()
launchOnFF k xdim  = CUDA.launchKernel k (8,8,xdim) (32,32,1) 0 Nothing

type DoubleDevPtr = CUDA.DevicePtr Double

foreign import ccall unsafe reduce_init :: IO CInt

launchReduce :: CxDoubleDevPtr -> DoubleDevPtr -> Int -> IO ()
launchReduce idata odata n = do
  r <- reduce_init
  CUDA.nothingIfOk $ toEnum $ fromIntegral r
  CUDA.sync
  CUDA.launchKernel reduce_512_e2 (n `div` 1024,1,1) (512,1,1) (fromIntegral $ 512 * sizeOf (undefined :: Double)) Nothing
    $ CUDA.mapArgs idata odata n

launchNormalize :: DoubleDevPtr -> CxDoubleDevPtr -> Int -> IO ()
launchNormalize normp ptr len =
  CUDA.launchKernel normalize (128,1,1) (512,1,1) 0 Nothing
    $ CUDA.mapArgs normp ptr len

generateGCFs :: GridPar -> GCFSet -> IO ()
generateGCFs gpar gcfSet = do

    -- Get parameters
    let over = gcfpOver $ gcfsPar gcfSet
        maxSize = gcfpMaxSize $ gcfsPar gcfSet
        -- half-width of the field in radians
        t2 = gridTheta gpar / 2

    -- Allocate & initialise work data structures
    let gridSize = maxSize*maxSize
        overGridSize = maxSize*maxSize*over*over
    CUDA.allocaArray gridSize $ \(ffp0 :: CxDoubleDevPtr) -> do {
    launchOnFF r2 1 $ CUDA.mapArgs ffp0 t2;

    CUDA.allocaArray gridSize $ \(ffpc :: CxDoubleDevPtr) -> do {
    CUDA.allocaArray overGridSize $ \(overo :: CxDoubleDevPtr) ->
    CUDA.allocaArray overGridSize $ \(overt :: CxDoubleDevPtr) ->
    CUDA.allocaArray (gridSize `div` 1024) $ \(normp :: DoubleDevPtr) ->
    forM_ (gcfs gcfSet) $ \gcf -> do

        -- Generate GCF kernel
        let w = gcfMidW gcf
        CUDA.sync
        launchOnFF wkernff 1 $ CUDA.mapArgs ffpc ffp0 w

        -- Copy to center of overo grid, fft and transpose
        let cxSize = sizeOf (undefined :: CxDouble)
        CUDA.memset (CUDA.castDevPtr overo) (fromIntegral $ overGridSize * cxSize) 0
        CUDA.sync
        launchOnFF copy_2_over 1 $ CUDA.mapArgs overo ffpc
        fft2dComplexDSqInplaceCentered Nothing Inverse (maxSize*over) overo
        launchOnFF transpose_over0 64 $ CUDA.mapArgs overt overo
        CUDA.sync

        -- Now extract oversampling kernels
        let supp = gcfSize gcf
            DeviceVector _ outv = gcfData gcf
        forM_ [0..over*over-1] $ \n -> do

            -- Reduce
            let layerp = overt `CUDA.advanceDevPtr` (n * gridSize)
            launchReduce layerp normp gridSize
            CUDA.sync

            -- CUDA.peekArray 1 normp hnormp -- DEBUG
            -- peek hnormp >>= print

            -- Normalise
            launchNormalize normp layerp gridSize
            CUDA.sync

            -- Extract a GCF of the appropriate size
            let outp = outv `CUDA.advanceDevPtr` (n * supp * supp)
            launchOnFF wextract1 1 $ CUDA.mapArgs outp layerp supp
            CUDA.sync
    } }

kernel :: GridPar
       -> GCFPar
       -> Vis
       -> IO GCFSet
kernel gp gcfp vis = do

    -- Make sure parameters make sense
    when (gcfpMaxSize gcfp /= 256) $
        fail "GCF max size must be 256 for this GCF generation kernel!"
    when (gcfpOver gcfp /= 8) $
        fail "Oversampling must be 8 for this GCF generation kernel!"

    -- Number of GCFs to generate
    let wstep = gcfpStepW gcfp
        wplanes :: Int
        wplanes = maximum [1, round (visMaxW vis / wstep), round (-visMinW vis / wstep)]
        over = gcfpOver gcfp

    -- Allocate GCFs, generate lookup table
    table <- allocCVector ((wplanes + 1) * over * over)
    gs <- forM [0..wplanes] $ \i -> do

        -- TODO: This is probably not the best way to select w values?
        let w = wstep * fromIntegral i
            wmin = w - wstep / 2
            wmax = w + wstep / 2
            size = min (gcfpMaxSize gcfp)
                       (gcfpMinSize gcfp + gcfpGrowth gcfp * abs i)

        v <- allocDeviceVector (size * size * over * over)

        -- Populate lookup table for all oversampling values
        forM_ [0..over*over-1] $ \j -> do
            let DeviceVector _ p = offsetVector v (size*size*j)
            pokeVector table (i*over*over+j) p

        return $ GCF w wmin wmax size v

    -- Generate GCFs
    gpuTable <- toDeviceVector table
    let gcfSet = GCFSet gcfp gs (castVector gpuTable)
    generateGCFs gp gcfSet

    -- Clean up?

    return gcfSet

