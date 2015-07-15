module Kernel.CPU.Hogbom where

import Foreign.Ptr
import Foreign.C

import Data
import Vector

foreign import ccall deconvolve ::
     Ptr Double -- Model
  -> Ptr Double -- Dirty image
  -> Ptr Double -- Psf
  -> CInt       -- Height
  -> CInt       -- Pitch
  -> CInt       -- Max number of iterations
  -> Double     -- Gain
  -> Double     -- Threshold
  -> IO ()

-- Trivial
cleanPrepare :: CleanPar -> Image -> IO Image
cleanPrepare _ psf = return psf

cleanKernel :: CleanPar -> Image -> Image -> IO (Image, Image)
cleanKernel (CleanPar iter gain thresh) res@(Image gp _ (CVector _ dirtyp)) (Image _ _ (CVector _ psfp)) = do
    mdl@(CVector _ modp) <- allocCVector (imageSize gp)
    deconvolve  modp dirtyp psfp (fi $ gridHeight gp) (fi $ gridPitch gp) (fi iter) gain thresh
    return (res, Image gp 0 mdl)
  where fi = fromIntegral
cleanKernel _ _ _ = error "Wrong image or psf location for CPU clean kernel"
