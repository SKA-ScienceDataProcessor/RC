
module Kernel where

import Data
import qualified Kernel.Dummy as Dummy

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
  , gridkGrid :: Vis -> GCFSet -> UVGrid -> IO UVGrid
    -- ^ Grid the visibilities to the "UVGrid" using the given set of
    -- grid convolution functions.
    --
    -- We guarantee that the "GCFSet" covers the @w@-range of the
    -- visibilities. The "UVGrid" is expected to be consumed in the
    -- process, so it is okay (and expected) to return the same grid
    -- with updated data.
  , gridkDegrid :: UVGrid -> GCFSet -> Vis -> IO Vis
    -- ^ Extract visibilities from the grid using the set of grid
    -- convolution functions. The output visibilities are guaranteed
    -- to have the same positions as the input visibilities.
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
  { -- | Produces all "GCF"s we will need for gridding data in the
    -- @w@-range indicated by the two "Double" values.
    gcfKernel :: GridPar
              -> GCFPar
              -> Double
              -> Double
              -> IO GCFSet
  }

-- | Cleaning kernel. This kernel is meant to run some variant of Hogbom's
-- CLEAN algorithm.
data CleanKernel = CleanKernel
  { -- | Runs Hogbom's algorithm. The returned values are the residual
    -- and the model image. The parameters are the cleaning
    -- parameters, the image to clean as well as a suitable point
    -- spread function (PSF).
    cleanKernel :: CleanPar -> Image -> Image -> IO (Image, Image)
  }

-- | The supported gridding kernels
gridKernels :: [(String, IO GridKernel)]
gridKernels =
  [ ("dummy", return $ GridKernel Dummy.prepareVis Dummy.prepareGCF Dummy.createGrid
                                  Dummy.grid Dummy.degrid)
  , ("gpu", do
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
dftKernels =
  [ ("dummy", return $ DFTKernel Dummy.dft Dummy.dfti) ]

-- The supported discrete fourier transformation kernels
gcfKernels :: [(String, IO GCFKernel)]
gcfKernels =
  [ ("dummy", return $ GCFKernel Dummy.gcf) ]

-- The supported discrete fourier transformation kernels
cleanKernels :: [(String, IO CleanKernel)]
cleanKernels =
  [ ("dummy", return $ CleanKernel Dummy.clean) ]

initKernel :: [(String, IO a)] -> String -> IO a
initKernel kernels name = case lookup name kernels of
  Just k  -> k
  Nothing -> fail $ "Could not find kernel " ++ name ++ "!"
