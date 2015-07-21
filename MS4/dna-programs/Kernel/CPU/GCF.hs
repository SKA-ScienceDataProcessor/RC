{-# LANGUAGE BangPatterns #-}

module Kernel.CPU.GCF (kernel) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
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

foreign import ccall calcAccums ::
     Ptr Double -- uvws
  -> Ptr Double -- sums (out)
  -> Ptr CInt   -- npts (out)
  -> Double     -- wstep
  -> CInt       -- # of points baselines * channels * timesteps
  -> CInt       -- # of planes
  -> IO ()

kernel ::
     GridPar
  -> GCFPar
  -> Vis
  -> IO GCFSet
kernel gp gcfp vis =
  allocaArray numOfPlanes $ \wsump ->
  allocaArray numOfPlanes $ \np ->
  allocaArray arenaSize $ \arenap -> do
    fftInitThreading
    gcfDataPtr <- mallocArray gcfDataSize
    gcfTablePtr <- mallocArray gcfTableSize

    calcAccums uvwp wsump np wstep (f $ visTimesteps vis * (length $ visBaselines vis)) (f numOfPlanes)
    -- I'm using an explicit recursion explicitly.
    -- Usually this generates much better code.
    -- OTOH, higher-order functions could potentially be fused.
    let
      mkLayers !p0 !destPtr !dTabPtr (s:ss) wsumpc npc wmid = do
        wsumc <- peek wsumpc
        nc <- peek npc
        let avg = if nc > 0 then wsumc/fromIntegral nc else wmid
        -- FIXME: w (avg here) should be scaled by exactly the same scale as uw's are before gridding!
        !p <- mkGCFLayer p0 destPtr dTabPtr arenap (f s) (f gcfMaxSize) (f pad) t2 avg
        mkLayers p (advancePtr destPtr $ over2 * s * s) (advancePtr dTabPtr over2) ss (advancePtr wsumpc 1) (advancePtr npc 1) (wmid + wstep)
      mkLayers _ _ _ [] _ _ _ = return ()
    --
    mkLayers nullPtr gcfDataPtr gcfTablePtr layerSupps wsump np (- wstep * fromIntegral maxWPlane)
    -- FIXME!!!: Change all data types in such a way
    --   that they would bring their own finalizers with them !!!
    -- ATM we hack freeGCFSet slightly (see Data.hs)
    return $ GCFSet gcfp [] (CVector gcfTableSize $ castPtr gcfTablePtr)
  where
    uvwp = case visPositions vis of
             CVector _ p -> castPtr p
             _ -> error "Wrong uvw location for CPU GCF."
    f = fromIntegral
    t2 = gridTheta gp / 2
    wstep = gcfpStepW gcfp
    over = gcfpOver gcfp
    over2 = over * over
    gcfMaxSize = gcfpMaxSize gcfp
    supp i = min gcfMaxSize
               (gcfpMinSize gcfp + gcfpGrowth gcfp * abs i)
    maxWPlane = max (round (visMaxW vis / wstep)) (round (-visMinW vis / wstep))
    numOfPlanes = 2*maxWPlane+1
    layerSupps = map supp [-maxWPlane .. maxWPlane]
    gcfDataSize = over2 * sum (map (\s -> s * s) layerSupps)
    gcfTableSize = over2 * (1 + 2 * maxWPlane)
    l = over * gcfMaxSize
    pad = 4
    pitch = l + pad
    arenaSize = l * pitch
