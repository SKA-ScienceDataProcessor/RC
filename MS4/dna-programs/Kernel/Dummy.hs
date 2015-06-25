
-- | This module implements dummy kernels, which implements kernel
-- functions in the easiest fashion possible. In most cases, this is
-- simply about returning an uninitiailised grid, or returning buffers
-- unchanged.
module Kernel.Dummy where

import Data
import Vector

import Control.Monad ( forM )
import Foreign.Ptr

-- Grid kernel definition

prepareVis :: Vis -> IO Vis
prepareVis = return

prepareGCF :: GCFSet -> IO GCFSet
prepareGCF = return

createGrid :: GridPar -> GCFPar -> IO UVGrid
createGrid gp _ = do
   dat <- allocCVector (gridHalfSize gp)
   return $ UVGrid gp 0 dat

grid :: Vis -> GCFSet -> UVGrid -> IO UVGrid
grid _ _ = return

degrid :: UVGrid -> GCFSet -> Vis -> IO Vis
degrid _ _ = return

-- DFT kernel definition

-- NOTE: We are simulating in-place FFT, which will return the same
-- pointer passed as a parameter. This must be a valid implementation
-- for in-place FFT to work!

dft :: Image -> IO UVGrid
dft (Image gp pad v) = do
    CVector n p <- toCVector v
    return $ UVGrid gp (pad `div` 2) (CVector n (castPtr p))

dfti :: UVGrid -> IO Image
dfti (UVGrid gp pad v) = do
    CVector n p <- toCVector v
    return $ Image gp (pad * 2) (CVector n (castPtr p))

-- GCF kernel definition

gcf :: GridPar -> GCFPar -> Double -> Double -> IO GCFSet
gcf _gp gcfp wlow whigh = do

    -- Number of positive / negative GCFs to generate
    let wstep = gcfpStepW gcfp
        neg, pos :: Int
        neg = max 1 $ ceiling (-wlow / wstep)
        pos = max 1 $ ceiling (whigh / wstep)

    -- Actually generate GCFs
    gs <- forM [-neg..pos] $ \i -> do
        let w = wstep * fromIntegral i
            wmin = w - wstep / 2
            wmax = w + wstep / 2
        v <- allocCVector (gcfpMaxSize gcfp * gcfpMaxSize gcfp)
        return $ GCF wmin wmax (gcfpMaxSize gcfp) v

    return $ GCFSet gcfp gs

-- GCF kernel definition

clean :: CleanPar -> Image -> Image -> IO (Image, Image)
clean _clp dirty _psf = do

    -- Create fresh image for the model
    modelData <- allocCVector (imageSize (imgPar dirty))
    let model = Image (imgPar dirty) 0 modelData

    -- Return the uncleaned image together with the model
    return (dirty, model)
