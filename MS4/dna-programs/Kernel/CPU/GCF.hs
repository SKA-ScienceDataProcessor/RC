{-# LANGUAGE BangPatterns #-}

module Kernel.CPU.GCF (kernel) where

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
  -> Ptr (Ptr (Complex Double)) -- table
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
kernel gp gcfp vis = allocaArray arenaSize $ \arenap -> do
    fftInitThreading
    gcfDataPtr <- mallocArray gcfDataSize
    gcfTablePtr <- mallocArray gcfTableSize
    -- I'm using an explicit recursion explicitly.
    -- Usually this generates much better code.
    -- OTOH, higher-order functions could potentially be fused.
    let
      mkLayers !p0 !destPtr !dTabPtr (s:ss) (w:ww) = do
        !p <- mkGCFLayer p0 destPtr dTabPtr arenap (f s) (f gcfMaxSize) (f pad) t2 w
        mkLayers p (advancePtr destPtr $ over2 * s * s) (advancePtr dTabPtr over2) ss ww
      mkLayers _ _ _ _ [] = return ()
      mkLayers _ _ _ [] _ = return ()
    --
    mkLayers nullPtr gcfDataPtr gcfTablePtr layerSizes ws
    -- FIXME!!!: Change all data types in such a way
    --   that they would bring their own finalizers with them !!!
    -- ATM we hack freeGCFSet slightly (see Data.hs)
    return $ GCFSet gcfp [] (CVector gcfTableSize $ castPtr gcfTablePtr)
  where
    f = fromIntegral
    t2 = gridTheta gp / 2
    wstep = gcfpStepW gcfp
    over = gcfpOver gcfp
    over2 = over * over
    gcfMaxSize = gcfpMaxSize gcfp
    size i = min gcfMaxSize
               (gcfpMinSize gcfp + gcfpGrowth gcfp * abs i)
    maxWPlane = max (round (visMaxW vis / wstep)) (round (-visMinW vis / wstep))
    planes = [-maxWPlane .. maxWPlane]
    layerSizes = map size planes
    ws = map ((wstep*). fromIntegral) planes
    gcfDataSize = over2 * sum layerSizes
    gcfTableSize = over2 * (1 + 2 * maxWPlane)
    l = over * gcfMaxSize
    pad = 4
    pitch = l + pad
    arenaSize = l * pitch
