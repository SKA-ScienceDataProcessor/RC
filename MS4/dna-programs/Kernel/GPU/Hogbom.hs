{-# LANGUAGE ScopedTypeVariables #-}

module Kernel.GPU.Hogbom ( cleanPrepare, cleanKernel ) where

import Control.Monad
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Storable ( sizeOf, peek )
import Foreign.C.Types

import Data
import Kernel.GPU.Common as CUDA
import Vector

type Peak = (Int, Int, Double)

cleanPrepare :: CleanPar -> Image -> IO Image
cleanPrepare _ psf = do

   -- Transfer PSF to GPU
   psfv <- toDeviceVector (imgData psf)
   let psf' = psf{ imgData = psfv }

   -- TODO: Cache peak?

   return psf'

cleanKernel :: CleanPar -> Image -> Image -> IO (Image, Image)
cleanKernel cleanp dirty psf = do

   -- Check configuration - the peak find kernel requires a quadratic
   -- grid without gaps.
   let width = gridWidth (imgPar dirty)
   when (width /= gridPitch (imgPar dirty) || width /= gridHeight (imgPar dirty)) $
     fail "Cleaning kernel assumes quadratic grid without internal padding!"
   -- Furthermore, the reduction requires the field size to be a power of 2.
   let powers = map (2^) [1..32 :: Int]
   when (width `notElem` powers) $
     fail "Cleaning kernel requires a grid size that is a power of two!"

   -- Transfer images, if required
   dirtyv <- toDeviceVector (imgData dirty)
   let dirty' = dirty{ imgData = dirtyv }

   -- Allocate model
   modelv <- allocCVector (2 * imageSize (imgPar dirty))
   let model = Image (imgPar dirty) 0 modelv

   -- Find peak in PSF
   (psfx, psfy, psfv) <- findPeak psf

   -- Minor cleaning loop
   let loop res 0    = return (res, model)
       loop res fuel = do

         -- Find peak in residual
         (resx, resy, resv) <- findPeak res

         -- Below threshold?
         if abs resv < cleanThreshold cleanp then do
           return (res, model)
          else do

           -- Subtract PSF
           let mval = cleanGain cleanp * resv / psfv
           res' <- subtractImg res psf (resx - psfx, resy - psfy) mval

           -- Update model, loop
           let ix = resx + resy * width
           mpx <- peekVector modelv ix
           pokeVector modelv ix (mpx + mval)
           loop res' (fuel-1)
   loop dirty' (cleanIter cleanp)

foreign import ccall unsafe findPeak_init :: IO CInt
foreign import ccall unsafe "&" findPeak_512_e2 :: Fun

-- | Number of blocks of a certain size required to cover data of a given size
blockCount :: Int -> Int -> Int
blockCount datSize blkSize = (datSize + blkSize - 1) `div` blkSize

-- | Finds the position with the highest intensity in the image
findPeak :: Image -> IO Peak
findPeak img = do

  -- Get data
  let DeviceVector _ imgp = imgData img

  -- Set up reduction kernel
  nothingIfOk . toEnum . fromIntegral =<< findPeak_init
  sync

  -- Run
  let width = gridWidth $ imgPar img
      placeSize = sizeOf (undefined :: CULong) + sizeOf (undefined :: Double)
      blocks = blockCount width 1024
  CUDA.allocaArray (placeSize * blocks)  $ \(workArea :: DevicePtr Word8) -> do
    launchKernel findPeak_512_e2
      (blocks, 1, 1) (512, 1, 1) (512 * fromIntegral placeSize) Nothing $
      mapArgs imgp workArea (width * width)
    sync

    -- Load final result value(s) from start of work area
    let peekd p = alloca (\p' -> peekArray 1 p p' >> peek p')
    pos <- peekd $ castDevPtr workArea :: IO Word64
    val <- peekd $ castDevPtr (workArea `plusDevPtr` sizeOf (undefined :: CULong))
    return (fromIntegral (pos `mod` fromIntegral width),
            fromIntegral (pos `div` fromIntegral width),
            val)

foreign import ccall unsafe "&" subtract_psf_kernel :: Fun

-- | Subtract two images from each other at an offset and
-- muliplier. The first image parameter is the one getting updated.
subtractImg :: Image -> Image -> (Int, Int) -> Double -> IO Image
subtractImg res psf (x,y) gain = do

  -- Calculate data movement distance and amount
  resv@(DeviceVector _ resp) <- toDeviceVector (imgData res)
  let DeviceVector _ psfp = imgData psf
      width = gridWidth (imgPar res)
      diff = x + y * width
      resp' = resp -- `plusDevPtr` max 0 diff
      psfp' = psfp -- `plusDevPtr` max 0 (-diff)
      stopx = width - abs x
      stopy = width - abs y
      blockDim = 16

  -- Run the kernel
  launchKernel subtract_psf_kernel
    (blockCount stopx blockDim, blockCount stopy blockDim, 1) (blockDim, blockDim, 1) 0 Nothing $
    mapArgs resp' psfp' gain diff  stopx stopy width
  sync

  -- Done
  return res{imgData=resv}
