
module Kernel.GPU.NvidiaDegrid ( degrid ) where

import Control.Monad
import Data.Complex

import Data
import Kernel.GPU.Common
import Vector

foreign import ccall unsafe "&" degrid_kernel :: Fun

degrid :: UVGrid -> GCFSet -> Vis -> IO Vis
degrid uvg gcfSet vis = do

   -- Check configuration - we assume a quadratic grid without gaps
   let width = gridWidth (uvgPar uvg)
   when (width /= gridPitch (uvgPar uvg) || width /= gridHeight (uvgPar uvg)) $
     fail "Degridding kernel assumes quadratic grid without internal padding!"
   when (gcfpOver (gcfsPar gcfSet) /= 8) $
     fail "Degridding kernel assumes an oversampling factor of 8!"

   -- Allocate output array, transfer grid
   visOut <- allocDeviceVector $ vectorSize $ visData vis :: IO (Vector (Complex Double))
   uvg'@(DeviceVector _ uvgp) <- toDeviceVector $ uvgData uvg

   -- Process visibilities by baselines
   forM_ (visBaselines vis) $ \bl -> do

     -- Get the appropriate GCF. Note that this kernel assumes that we
     -- are only using one GCF per baseline... To improve data quality
     -- we might have to split the baseline in future...
     let w = (vblMinW bl + vblMaxW bl) / 2
         Just gcf = findGCF gcfSet w
         gcf_size = gcfSize gcf

     -- The nVidia kernel expects to get a pointer to the middle of
     -- the GCF.
     let gcf_off = (gcf_size+1) * gcf_size `div` 2
         DeviceVector _ gcfp = offsetVector (gcfData gcf) gcf_off

     -- Visibility pointers
     let offset = vblOffset bl
         DeviceVector _ outp = offsetVector visOut offset
         DeviceVector _ posp = offsetVector (visPositions vis) offset
         npts = vblPoints bl

     -- Call the kernel
     launchKernel degrid_kernel
       (npts `div` 32, 1, 1) (32, 8, 1) 0 Nothing $
       mapArgs outp posp uvgp gcfp npts width gcf_size
     sync

   -- Free grid
   freeVector (uvgData uvg)

   -- TODO: Subtract visibilities!
   return vis
