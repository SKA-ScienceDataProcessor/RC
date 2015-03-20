module FFT where

import Foreign.CUDA.Driver (
    Fun
  , launchKernel
  , FunParam(..)
  )
import Foreign.CUDA.Types (Stream)
import Foreign.CUDA.Ptr (DevicePtr)
import Foreign.CUDA.FFT

data Mode =
    Forward
  | Reverse
  | Inverse

fft2dComplexDSqInplace :: Maybe Stream -> Mode -> Int -> DevicePtr Double -> IO ()
fft2dComplexDSqInplace mbstream mode size inoutp = do
    handle <- plan2D size size Z2Z
    case mbstream of
      Just str -> setStream handle str
      _ -> return ()
    execZ2Z handle inoutp inoutp (signOfMode mode)
    destroy handle
  where
    signOfMode Forward = -1
    signOfMode Reverse =  1
    signOfMode Inverse =  1


fft2dComplexDSqInplaceCentered :: Maybe Stream -> Mode -> Int -> DevicePtr Double -> Fun -> Fun -> IO ()
fft2dComplexDSqInplaceCentered mbstream mode size inoutp ifftshift fftshift = do
    mkshift ifftshift
    fft2dComplexDSqInplace mbstream mode size inoutp
    mkshift fftshift
  where
    threads_per_dim = min size 16
    blocks_per_dim = (size + threads_per_dim - 1) `div` threads_per_dim
    mkshift sfun =
      launchKernel sfun (blocks_per_dim, blocks_per_dim, 1) (threads_per_dim, threads_per_dim, 1)
        0 mbstream [VArg inoutp, IArg $ fromIntegral size]
