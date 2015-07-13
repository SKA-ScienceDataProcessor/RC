{-# LANGUAGE CPP #-}

module Kernel.GPU.FFT
  ( DftPlans
  , dftPrepare
  , dftClean
  , dftIKernel
  , dftKernel
    -- * For use by other kernels
  , Mode(..)
  , fft2dComplexDSqInplace
  , fft2dComplexDSqInplaceCentered
  ) where

import Control.Monad (when)
import Data.Complex
import Data.IORef
import Foreign.CUDA.FFT
import Foreign.Storable
import Foreign.Storable.Complex ()

import Data
import Kernel.GPU.Common
import Vector

data Mode =
    Forward
  | Reverse
  | Inverse

fft2dComplexDSqInplace :: Maybe Stream -> Mode -> Int -> CxDoubleDevPtr -> IO ()
fft2dComplexDSqInplace mbstream mode size inoutp = do
    handle <- plan2D size size Z2Z
    case mbstream of
      Just str -> setStream handle str
      _ -> return ()
    execZ2Z handle (castDevPtr inoutp) (castDevPtr inoutp) (signOfMode mode)
    destroy handle
  where
    signOfMode Forward = -1
    signOfMode Reverse =  1
    signOfMode Inverse =  1

fft2dComplexDSqInplaceCentered :: Maybe Stream -> Mode -> Int -> CxDoubleDevPtr -> IO ()
fft2dComplexDSqInplaceCentered mbstream mode size inoutp = do
    fftShiftCx False mbstream size size inoutp
    fft2dComplexDSqInplace mbstream mode size inoutp
    fftShiftCx True mbstream size size inoutp

foreign import ccall unsafe "&" ifftshift_kernel_cx :: Fun
foreign import ccall unsafe "&" fftshift_kernel_cx :: Fun
foreign import ccall unsafe "&" ifftshift_kernel_r :: Fun
foreign import ccall unsafe "&" fftshift_kernel_r :: Fun

-- | Shift (0/0) to either the top-left corner or the center of the
-- image. Normally, we treat buffers as centered, but for FFT we need
-- to convert data so our (0/0) point is, in fact, at the beginning of
-- the buffer.
--
-- TODO: The "proper" cuFFT way of doing this is probably callbacks.
fftShiftCx :: Bool -> Maybe Stream -> Int -> Int -> CxDoubleDevPtr -> IO ()
fftShiftCx inv = fftShiftGen (if inv then ifftshift_kernel_cx else fftshift_kernel_cx)

-- | As "fftShiftCx", but for real "Double" values
fftShiftR :: Bool -> Maybe Stream -> Int -> Int -> DevicePtr Double -> IO ()
fftShiftR inv = fftShiftGen (if inv then ifftshift_kernel_r else fftshift_kernel_r)

-- | Non-typesafe version of the above
fftShiftGen :: Fun -> Maybe Stream -> Int -> Int -> DevicePtr a -> IO ()
fftShiftGen fun mbstream size pitch inoutp =
    launchKernel fun (blocks_per_dim0, blocks_per_dim1, 1) (threads_per_dim, threads_per_dim, 1)
                 0 mbstream $ mapArgs inoutp size pitch
  where
    threads_per_dim = min size 16
    blocks_per_dim0 = (size + threads_per_dim - 1) `div` threads_per_dim
    blocks_per_dim1 = (size - 1) `div` (threads_per_dim * 2) + 1

-- | DFT plans. These are used to communicate the plan data between
-- the DFT kernels.
data DftPlans = DftPlans
  { dftPlan :: Handle
  , dftIPlan :: Handle
  }

dftPrepare :: IORef DftPlans -> GridPar -> IO ()
dftPrepare plans gridp = do

  -- Memory layouts for image and uv-grid data. We do this mainly so
  -- cuFFT knows about the pitch. The third "distance" parameter is
  -- only used for batching, which will not actually do.
  let width = gridWidth gridp; height = gridHeight gridp; pitch = gridPitch gridp
      imgLayout = ([height, pitch], 1, pitch * height)
      uvgLayout = ([height, pitch], 1, pitch * height)
  when (width /= height) $
    fail "DFT kernel assumes that the grid is quadratic!"

  -- Create plans
  plan <- planMany [height, width] (Just imgLayout) (Just uvgLayout) D2Z 1
  iplan <- planMany [height, width] (Just uvgLayout) (Just imgLayout) Z2D 1
  writeIORef plans $ DftPlans plan iplan

dftClean :: IORef DftPlans -> IO ()
dftClean plans = do
  DftPlans plan iplan <- readIORef plans
  destroy plan
  destroy iplan
  writeIORef plans (error "DFT kernel called without dftPrepare!")

dftKernel :: IORef DftPlans -> Image -> IO UVGrid
dftKernel plans img = do

  -- Parameters
  let height = gridHeight (imgPar img); pitch = gridPitch (imgPar img)
  DftPlans{dftPlan=plan} <- readIORef plans

  -- Get image data
  DeviceVector n ptr <- toDeviceVector (imgData img)
  let padptr = ptr `plusDevPtr` imgPadding img

  -- Image dimensions
  let cxdSize = sizeOf (undefined :: Complex Double)
      dSize = sizeOf (undefined :: Double)
      n' = n * dSize `div` cxdSize

  let pad' = imgPadding img * dSize `div` cxdSize
      ptr' = castDevPtr ptr
      padptr' = ptr' `plusDevPtr` pad'

  -- Make sure buffers are big enough for in-place transformation
  when (n' < height * pitch) $
    fail "DFT kernel got an image buffer too small to perform in-place real-to-complex DFT!"

  -- Perform in-place fourier transformation
  fftShiftR False Nothing  height pitch padptr
  execD2Z plan padptr (castDevPtr padptr')
  fftShiftCx False Nothing  height pitch padptr'
  sync

  return $ UVGrid (imgPar img) pad' (DeviceVector n' ptr')

dftIKernel :: IORef DftPlans -> UVGrid -> IO Image
dftIKernel plans uvg = do

  -- Parameters
  let height = gridHeight (uvgPar uvg); pitch = gridPitch (uvgPar uvg)
  DftPlans{dftIPlan=iplan} <- readIORef plans

  -- Get grid data
  DeviceVector n ptr <- toDeviceVector (uvgData uvg)
  let padptr = ptr `plusDevPtr` uvgPadding uvg

  -- Image dimensions
  let cxdSize = sizeOf (undefined :: Complex Double)
      dSize = sizeOf (undefined :: Double)
      n' = n * cxdSize `div` dSize

  let pad' = uvgPadding uvg * cxdSize `div` dSize
      ptr' = castDevPtr ptr
      padptr' = ptr' `plusDevPtr` pad'

  -- Perform in-place fourier transformation
  fftShiftCx False Nothing  height pitch padptr
  execZ2D iplan (castDevPtr padptr) padptr'
  fftShiftR False Nothing  height pitch padptr'
  sync

  return $ Image (uvgPar uvg) pad' (DeviceVector n' ptr')
