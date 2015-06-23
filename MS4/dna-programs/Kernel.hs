
module Kernel where

import Data

-- | Gridding kernels deal with adding or extracting visibilities
-- to/from an "UVGrid"
data GridKernel = GridKernel
  { gridkPrepareVis :: Vis -> IO Vis
    -- ^ Prepare visibilities for gridding. This can transfer the
    -- visibility data into another memory region and/or calculate
    -- required binning data.
  , gridkPrepareGCF :: GCFSet -> IO GCFSet
    -- ^ Prepare the grid convolution function for gridding. Might,
    -- again, involve memory transfer.
  , gridkCreateGrid :: GridPar -> GCFPar -> IO UVGrid
    -- ^ Produce a new UV grid suitable for gridding
  , gridkGrid :: Vis  -- ^ Visibilities to grid. Note that with big @GCF@,
                      -- the visibilities might be substantially outside of
                      -- the tile's @uv@ range.
              -> GCFSet -- ^ The GCF set to use. We guarantee that it
                        -- covers the @w@-range of the visibilities.
              -> UVGrid -- ^ Grid to add visibilities to. Expected to
                        -- be consumed in the process, so it is okay
                        -- (and expected) to return the same updated grid.
              -> IO UVGrid
    -- ^ Grid the visibilities to the tile using the convolution function.
  , gridkDegrid :: UVGrid
                -> GCFSet
                -> Vis    -- ^ Positions to degrid at (disregarding any contained data).
                -> IO Vis
    -- ^ Extract visibilities from the grid using the convolution function.
    -- We guarantee that the visibilities are in a @w@-plane appropriate
    -- for the @GCF@. However, visibilities might generally lie outside
    -- the @uv@ range of the tile.
  }

-- | Fourier transformation kernels. These allow us to obtain "Image"
-- from "UVGrid" as well as the other way around.
data DFTKernel = DFTKernel
  { -- | Transforms an image into an uv-grid. This constitutes a
    -- discrete fourier transformation. We assume that the image is
    -- freed by the kernel.
    dftKernel :: Image -> IO UVGrid
    -- | Transforms an uv-grid into an image. This requires
    -- an inverse discrete fourier transformation. We assume that the
    -- grid is freed by the kernel.
  , dftIKernel :: UVGrid -> IO Image
  }

-- | Grid convolution generator kernel.
data GCFKernel = GCFKernel
  { -- | Produces all GCFs we will need for gridding data in the
    -- indicated @w@-range.
    gcfKernel :: GridPar
              -> GCFPar -- ^ Parameters to GCF generation, such as @w@ step
              -> Double -- ^ Minimum @w@ value to generate GCF for
              -> Double -- ^ Maximum @w@ value to generate GCF for
              -> IO GCFSet
  }

-- | Cleaning kernel. This kernel is meant to run some variant of Hogbom's
-- CLEAN algorithm.
data CleanKernel = CleanKernel
  { -- | Runs Hogbom's algorithm. The returned values are the residual
    -- and the model image.
    cleanKernel :: Int    -- ^ Maximum number of iterations in the minor loop
                -> Double -- ^ Gain (?)
                -> Double -- ^ Threshold for residual sum to exit clean early
                -> Image  -- ^ The dirty image to clean
                -> Image  -- ^ The point spread function (PSF)
                -> IO (Image, Image)
  }

-- | The supported gridding kernels
gridKernels :: [(String, IO GridKernel)]
gridKernels =
  [ ("gpu", do
        -- ...
        let prepareVis = undefined -- transfer to GPU, possibly sort
            prepareGCF = undefined -- transfer to GPU
            createGrid = undefined -- create GPU buffer
            grid = undefined -- some kernel, possibly depending on "opts"
            degrid = undefined -- nVidia's kernel?
        return (GridKernel prepareVis prepareGCF createGrid grid degrid)
    )
  , ("cpu", do
        -- ...
        let prepareVis = undefined
            prepareGCF = undefined
            createGrid = undefined
            grid = undefined
            degrid = undefined
        return (GridKernel prepareVis prepareGCF createGrid grid degrid)
    )
  ]

-- The supported discrete fourier transformation kernels
dftKernels :: [(String, IO DFTKernel)]
dftKernels = undefined

-- The supported discrete fourier transformation kernels
gcfKernels :: [(String, IO GCFKernel)]
gcfKernels = undefined

-- The supported discrete fourier transformation kernels
cleanKernels :: [(String, IO CleanKernel)]
cleanKernels = undefined

initKernel :: [(String, IO a)] -> String -> IO a
initKernel kernels name = case lookup name kernels of
  Just k  -> k
  Nothing -> fail $ "Could not find kernel " ++ name ++ "!"
