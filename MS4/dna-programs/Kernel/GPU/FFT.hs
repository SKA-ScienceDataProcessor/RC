{-# LANGUAGE CPP #-}

module Kernel.GPU.FFT where

import Foreign.CUDA.FFT

import Kernel.GPU.Common

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

#define __k(n) foreign import ccall unsafe "&" n :: Fun
__k(ifftshift_kernel)
__k(fftshift_kernel)

fft2dComplexDSqInplaceCentered :: Maybe Stream -> Mode -> Int -> CxDoubleDevPtr -> IO ()
fft2dComplexDSqInplaceCentered mbstream mode size inoutp = do
    mkshift ifftshift_kernel
    fft2dComplexDSqInplace mbstream mode size inoutp
    mkshift fftshift_kernel
  where
    threads_per_dim = min size 16
    blocks_per_dim0 = (size + threads_per_dim - 1) `div` threads_per_dim
    blocks_per_dim1 = (size - 1) `div` (threads_per_dim * 2) + 1
    mkshift sfun =
      launchKernel sfun (blocks_per_dim0, blocks_per_dim1, 1) (threads_per_dim, threads_per_dim, 1)
        0 mbstream $ mapArgs inoutp size

fftGridPolarization :: CxDoubleDevPtr -> IO ()
fftGridPolarization polp = fft2dComplexDSqInplaceCentered Nothing Forward gridsize polp
  where
    gridsize = 4096
