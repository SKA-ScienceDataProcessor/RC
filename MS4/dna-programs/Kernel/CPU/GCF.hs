{-# LANGUAGE BangPatterns #-}

module Kernel.CPU.GCF (kernel) where

import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.Complex

import Kernel.CPU.FFT
import Data
import Vector

foreign import ccall mkGCFLayer ::
     FftPlan
  -> Ptr (Complex Double) -- destination buffer
  -> Ptr (Complex Double) -- work buffer
  -> CInt                 -- support size to extract
  -> CInt                 -- maximum support size
  -> CInt                 -- lead dim padding
  -> Double               -- Theta/2
  -> Double               -- w
  -> IO FftPlan

kernel ::
     GridPar
  -> GCFPar
  -> Vis
  -> IO GCFSet
kernel _gp gcfp vis = allocaArray arenaSize $ \arenap -> do
    fftInitThreading
    gcfDataPtr <- mallocArray gcfDataSize
    gcfTablePtr <- mallocArray gcfTableSize
    -- UNFINISHED !!! FIXME: 2 nonsence calls only to fix types!
    _ <- mkGCFLayer nullPtr gcfDataPtr arenap 0 0 0 0 0
    poke gcfTablePtr gcfDataPtr
    {- UNFINISHED !!!
    let
      mkLayers !p0 !destPtr !dTabPtr !(!s:!ss) = do
        !p <- mkGCFLayer p0 !destPtr arenap s gcfMaxSize pad ... ...
     -}
    -- FIXME!!!: Change all data types in such a way
    --   that they would bring their own finalizers with them !!!
    -- No global finalizers anymore!
    return $ GCFSet gcfp [] (CVector gcfTableSize $ castPtr gcfTablePtr)
  where
    wstep = gcfpStepW gcfp
    over = gcfpOver gcfp
    over2 = over * over
    gcfMaxSize = gcfpMaxSize gcfp
    size i = min gcfMaxSize
               (gcfpMinSize gcfp + gcfpGrowth gcfp * abs i)
    maxWPlane = max (round (visMaxW vis / wstep)) (round (-visMinW vis / wstep))
    layerSizes = map size [-maxWPlane .. maxWPlane]
    gcfDataSize = over2 * sum layerSizes
    gcfTableSize = over2 * (1 + 2 * maxWPlane)
    l = over * gcfMaxSize
    pad = 4
    pitch = l + pad
    arenaSize = l * pitch
