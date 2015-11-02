{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

-- | Data representation definitions
module Kernel.Data
  ( Config(..), GridPar(..), GCFPar(..)
  , Tag, Vis, UVGrid, Image, GCFs
  , UVGRepr, ImageRepr, PlanRepr, RawVisRepr, VisRepr, GCFsRepr
  , uvgRepr, imageRepr, planRepr, rawVisRepr, visRepr, gcfsRepr
  ) where

import Data.Typeable

import Flow.Halide
import Flow.Domain
import Flow.Kernel

data Config = Config
  { cfgInput  :: FilePath
  , cfgPoints :: Int
  , cfgOutput :: FilePath
  , cfgGrid   :: GridPar
  , cfgGCF    :: GCFPar
  }
data GridPar = GridPar
  { gridWidth :: !Int  -- ^ Width of the uv-grid/image in pixels
  , gridHeight :: !Int -- ^ Neight of the uv-grid/image in pixels
  , gridPitch :: !Int  -- ^ Distance between rows in grid storage. Can
                       -- be larger than width if data is meant to be
                       -- padded.
  , gridTheta :: !Double  -- ^ Size of the field of view in radians
  }
data GCFPar = GCFPar
  { gcfSize :: Int
  , gcfOver :: Int
  , gcfFile :: FilePath
  }

-- Data tags
data Tag -- ^ Initialisation (e.g. FFT plans)
data Vis -- ^ Visibilities (File name to OSKAR / raw visibilities / binned ...)
data UVGrid -- ^ UV grid
data Image -- ^ Image
data GCFs -- ^ A set of GCFs

deriving instance Typeable Tag
deriving instance Typeable Vis
deriving instance Typeable UVGrid
deriving instance Typeable Image
deriving instance Typeable GCFs

type UVGRepr = HalideRepr Dim3 Double UVGrid
uvgRepr :: GridPar -> UVGRepr
uvgRepr gp = halideRepr $ dimY gp :. dimX gp :. dimCpx :. Z

type ImageRepr = HalideRepr Dim2 Double Image
imageRepr :: GridPar -> ImageRepr
imageRepr gp = halideRepr $ dimY gp :. dimX gp :. Z

dimX :: GridPar -> Dim
dimX gp = (0, fromIntegral $ gridWidth gp)

dimY :: GridPar -> Dim
dimY gp = (0, fromIntegral $ gridHeight gp)

dimCpx :: Dim
dimCpx = (0, 2)

type PlanRepr = NoRepr Tag -- HalideRepr Dim0 Int32 Tag
planRepr :: PlanRepr
planRepr = NoRepr -- halideRepr dim0

type RawVisRepr = DynHalideRepr Dim1 Double Vis
rawVisRepr :: Domain Range -> RawVisRepr
rawVisRepr = dynHalideRepr (dim1 dimVisFields)

type VisRepr = DynHalideRepr Dim1 Double Vis
visRepr :: Domain Range -> VisRepr
visRepr = dynHalideRepr (dim1 dimVisFields)

-- | We have 5 visibility fields: Real, imag, u, v and w.
dimVisFields :: Dim
dimVisFields = (0, 5)

type GCFsRepr = HalideRepr Dim4 Double GCFs
gcfsRepr :: GCFPar -> GCFsRepr
gcfsRepr gcfp = halideRepr $ dimOver :. dimSize :. dimSize :. dimCpx :. Z
  where dimOver = (0, fromIntegral $ gcfOver gcfp * gcfOver gcfp)
        dimSize = (0, fromIntegral $ gcfSize gcfp)
