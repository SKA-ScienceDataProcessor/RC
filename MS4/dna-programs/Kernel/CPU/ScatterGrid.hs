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
  -> IO ()

{-
foreign import ccall "& gridKernelCPUHalfGCF" gridKernelCPUHalfGCF_ptr :: FunPtr CPUGridderType
foreign import ccall "& gridKernelCPUFullGCF" gridKernelCPUFullGCF_ptr :: FunPtr CPUGridderType

foreign import ccall "dynamic" mkCPUGridderFun :: FunPtr CPUGridderType -> CPUGridderType
-}

foreign import ccall gridKernelCPUFullGCF :: CPUGridderType
foreign import ccall deGridKernelCPUFullGCF :: CPUGridderType

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
gridWrapper gfun (Vis _ _ tsteps bls (CVector _ uwpptr) (CVector _ ampptr) _ _) (GCFSet gcfp _ (CVector _ table)) (UVGrid gp _ (CVector _ gptr)) =
    withArray supps $ \suppp -> 
      withArray uvws $ \uvwp -> 
        withArray amps $ \ampp -> 
          gfun scale wstep (fi $ length bls) suppp gptr table (castPtr uvwp) ampp (fi tsteps) (fi grWidth) (fi $ gridPitch gp)
          -- NOTE: Remember about normalization!
  where
    fi = fromIntegral
    grWidth = gridWidth gp
    scale = fromIntegral grWidth / gridLambda gp
    wstep = gcfpStepW gcfp
    size i = min (gcfpMaxSize gcfp) (gcfpMinSize gcfp + gcfpGrowth gcfp * i)
    supps = map (fi . size . baselineMinWPlane wstep) bls
    uvws = map (advancePtr uwpptr . vblOffset) bls
    amps = map (advancePtr ampptr . vblOffset) bls
gridWrapper _ _ _ _ = error "Wrong Vis or GCF or Grid location for CPU."

grid :: Vis -> GCFSet -> UVGrid -> IO UVGrid
grid vis gcfset uvg = gridWrapper gridKernelCPUFullGCF vis gcfset uvg >> return uvg

degrid :: UVGrid -> GCFSet -> Vis -> IO Vis
degrid uvg gcfset vis = gridWrapper deGridKernelCPUFullGCF vis gcfset uvg >> return vis
