module Kernel.CPU.ScatterGrid where

import Data.Complex
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array

import Data
import Vector

type PD = Ptr Double
type PCD = Ptr (Complex Double)

type CPUGridderType =
     -- Use Double instead of CDouble to reduce clutter
     Double   -- scale
  -> Double   -- wstep
  -> CInt      -- # of baselines
  -> Ptr CInt  -- baselines supports vector
  -> PCD       -- grid
  -> Ptr PCD   -- GCF layers pointer
  -> Ptr PD    -- baselines' uvw data
  -> Ptr PCD   -- baselines' vis data
  -> CInt      -- length of baselines vectors
  -> CInt      -- grid pitch
  -> CInt      -- grid size
  -> Ptr CInt  -- GCF supports vector
  -> IO ()

{-
foreign import ccall "& gridKernelCPUHalfGCF" gridKernelCPUHalfGCF_ptr :: FunPtr CPUGridderType
foreign import ccall "& gridKernelCPUFullGCF" gridKernelCPUFullGCF_ptr :: FunPtr CPUGridderType

foreign import ccall "dynamic" mkCPUGridderFun :: FunPtr CPUGridderType -> CPUGridderType
-}

foreign import ccall gridKernelCPUFullGCF :: CPUGridderType
foreign import ccall deGridKernelCPUFullGCF :: CPUGridderType

foreign import ccall "normalizeCPU" normalize :: PCD -> CInt -> CInt -> IO ()

-- trivial
-- we make all additional things (pregridding and rotation) inside the gridder
prepare :: GridPar -> Vis -> GCFSet -> IO (Vis, GCFSet)
prepare gp v gs
  | gridHeight gp /= gridWidth gp = error "Assume CPU grid is square!"
  | otherwise = return (v, gs)

-- Need no zero data
createGrid :: GridPar -> GCFPar -> IO UVGrid
createGrid gp _ = fmap (UVGrid gp 0) $ allocCVector (gridFullSize gp)

gridWrapper :: CPUGridderType -> Vis -> GCFSet -> UVGrid -> IO ()
-- This massive nested pattern matches are not quite flexible, but I think a burden
--   to adapt them if data types change is small, thus we stick to this more concise code ATM.
gridWrapper gfun (Vis vmin vmax tsteps bls (CVector _ uwpptr) (CVector _ ampptr) _ _) (GCFSet gcfp _ (CVector tsiz table)) (UVGrid gp _ (CVector _ gptr)) =
    withArray supps $ \suppp -> 
      withArray uvws $ \uvwp -> 
        withArray amps $ \ampp -> do
          withArray gcfSupps $ \gcfsp -> do
            gfun scale wstep (fi $ length bls) suppp gptr (advancePtr table $ tsiz `div` 2) (castPtr uvwp) ampp (fi tsteps) (fi grWidth) (fi $ gridPitch gp) (advancePtr gcfsp maxWPlane)
  where
    fi = fromIntegral
    grWidth = gridWidth gp
    scale = fromIntegral grWidth / gridLambda gp
    wstep = gcfpStepW gcfp
    size i = min (gcfpMaxSize gcfp) (gcfpMinSize gcfp + gcfpGrowth gcfp * i)
    supps = map (fi . size . baselineMinWPlane wstep) bls
    uvws = map (advancePtr uwpptr . vblOffset) bls
    amps = map (advancePtr ampptr . vblOffset) bls
    maxWPlane = max (round (vmax / wstep)) (round (-vmin / wstep))
    gcfSupps = map (fi . size) [-maxWPlane .. maxWPlane]
gridWrapper _ _ _ _ = error "Wrong Vis or GCF or Grid location for CPU."

grid :: Vis -> GCFSet -> UVGrid -> IO UVGrid
grid vis gcfset uvg = do
    gridWrapper gridKernelCPUFullGCF vis gcfset uvg
    normalize gptr (fi $ gridHeight gp) (fi $ gridPitch gp)
    return uvg
  where
    CVector _ gptr = uvgData uvg
    fi = fromIntegral
    gp = uvgPar uvg

-- What about the normalization here?
degrid :: UVGrid -> GCFSet -> Vis -> IO Vis
degrid uvg gcfset vis = gridWrapper deGridKernelCPUFullGCF vis gcfset uvg >> return vis
