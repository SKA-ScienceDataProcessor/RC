
module Kernel where

import Data.IORef

import Data

import qualified Kernel.Dummy as Dummy
import qualified Kernel.GPU.GCF as GPU_GCF
import qualified Kernel.GPU.ScatterGrid as GPU_ScatterGrid
import qualified Kernel.GPU.FFT as GPU_FFT
import qualified Kernel.GPU.Hogbom as GPU_Hogbom

import qualified Kernel.CPU.GCF as CPU_GCF
import qualified Kernel.CPU.ScatterGrid as CPU_ScatterGrid
import qualified Kernel.CPU.FFT as CPU_FFT
import qualified Kernel.CPU.Hogbom as CPU_Hogbom


-- | Gridding kernels deal with adding or extracting visibilities
-- to/from an "UVGrid"
data GridKernel = GridKernel
  { gridkPrepare :: GridPar -> Vis -> GCFSet -> IO (Vis, GCFSet)
    -- ^ Prepare visibilities and GCFs for gridding. This can transfer
    -- the visibility data into another memory region and/or calculate
    -- required binning data.
  , gridkCreateGrid :: GridPar -> GCFPar -> IO UVGrid
    -- ^ Produce a new UV grid suitable for gridding
  , gridkPhaseRotate :: GridPar -> Vis -> IO Vis
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
    -- convolution functions, and subtract them from the given
    -- visibility data.
  }

-- | Fourier transformation kernels. These allow us to obtain "Image"
-- from "UVGrid" as well as the other way around.
data DFTKernel = DFTKernel
  { -- | Prepare fourier kernel for handling data of the given shape.
    dftPrepare :: GridPar -> IO ()
    -- | Free preparation data
  , dftClean :: IO ()
    -- | Transforms an image into an uv-grid. This constitutes a
    -- discrete fourier transformation. We assume that the image is
    -- freed by the kernel.
  , dftKernel :: Image -> IO UVGrid
    -- | Transforms an uv-grid into an image. This requires
    -- an inverse discrete fourier transformation. We assume that the
    -- grid is freed by the kernel.
  , dftIKernel :: UVGrid -> IO Image
  }

-- | Grid convolution generator kernel.
data GCFKernel = GCFKernel
  { -- | Produces all "GCF"s we will need for gridding the given
    -- visibility data.
    gcfKernel :: GridPar
              -> GCFPar
              -> Vis
              -> IO GCFSet
  }

-- | Cleaning kernel. This kernel is meant to run some variant of Hogbom's
-- CLEAN algorithm.
data CleanKernel = CleanKernel
  { -- | Prepare cleaning algorithm for using the specified point
    -- spread function. This allows for transfering the image data and
    -- caching peak information.
    cleanPrepare :: CleanPar -> Image -> IO Image
    -- | Runs Hogbom's algorithm. The returned values are the residual
    -- and the model image. The parameters are the cleaning
    -- parameters, the image to clean as well as a suitable point
    -- spread function (PSF).
  , cleanKernel :: CleanPar -> Image -> Image -> IO (Image, Image)
  }

-- | The supported gridding kernels
gridKernels :: [(String, IO GridKernel)]
gridKernels =
  [ ("dummy", return $ GridKernel Dummy.prepare Dummy.createGrid Dummy.phaseRotate
                                  Dummy.grid Dummy.degrid)
  , ("gpu_scatter",
      return $ GridKernel GPU_ScatterGrid.prepare
                          GPU_ScatterGrid.createGrid
                          GPU_ScatterGrid.phaseRotate
                          GPU_ScatterGrid.grid
                          GPU_ScatterGrid.degrid
    )
  , ("cpu_scatter",
      return $ GridKernel CPU_ScatterGrid.prepare
                          CPU_ScatterGrid.createGrid
                          CPU_ScatterGrid.phaseRotate
                          CPU_ScatterGrid.grid
                          CPU_ScatterGrid.degrid
    )
  ]

-- The supported discrete fourier transformation kernels
dftKernels :: [(String, IO DFTKernel)]
dftKernels =
  [ ("dummy", return $ DFTKernel (\_ -> return ()) (return ()) Dummy.dft Dummy.dfti)
  , ("cufft", do
      plans <- newIORef (error "DFT kernel called without dftPrepare!")
      return $ DFTKernel (GPU_FFT.dftPrepare plans)
                         (GPU_FFT.dftClean plans)
                         (GPU_FFT.dftKernel plans)
                         (GPU_FFT.dftIKernel plans))
  , ("fftw3", do
      plans <- newIORef (error "DFT kernel called without dftPrepare!")
      return $ DFTKernel (CPU_FFT.dftPrepare plans)
                         (CPU_FFT.dftClean plans)
                         (CPU_FFT.dftKernel plans)
                         (CPU_FFT.dftIKernel plans))
  ]

-- The supported discrete fourier transformation kernels
gcfKernels :: [(String, IO GCFKernel)]
gcfKernels =
  [ ("dummy", return $ GCFKernel Dummy.gcf)
  , ("gpu", return $ GCFKernel GPU_GCF.kernel)
  , ("cpu", return $ GCFKernel CPU_GCF.kernel)
  ]

-- The supported discrete fourier transformation kernels
cleanKernels :: [(String, IO CleanKernel)]
cleanKernels =
  [ ("dummy", return $ CleanKernel Dummy.cleanPrepare Dummy.clean)
  , ("gpu_hogbom", return $ CleanKernel GPU_Hogbom.cleanPrepare
                                        GPU_Hogbom.cleanKernel)
  , ("cpu_hogbom", return $ CleanKernel CPU_Hogbom.cleanPrepare
                                        CPU_Hogbom.cleanKernel)
  ]

initKernel :: [(String, IO a)] -> String -> IO a
initKernel kernels name = case lookup name kernels of
  Just k  -> k
  Nothing -> fail $ "Could not find kernel " ++ name ++ "!"
