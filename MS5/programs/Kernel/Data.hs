{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

-- | Data representation definitions
module Kernel.Data
  ( Config(..), GridPar(..), GCFPar(..)
  , Tag, Vis, UVGrid, Image, GCFs
  , gridImageWidth, gridImageHeight
  -- * Data representations
  , UDom, VDom, WDom
  , UVGRepr, UVGMarginRepr, FacetRepr, ImageRepr, PlanRepr, GCFsRepr
  , uvgRepr, uvgMarginRepr, facetRepr, imageRepr, planRepr, gcfsRepr
  -- * Visibility data representations
  , RawVisRepr, RotatedVisRepr, VisRepr
  , rawVisRepr, rotatedVisRepr, visRepr
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
  , gridFacets :: !Int -- ^ Number of facets in X and Y directions
  , gridTheta :: !Double  -- ^ Size of the field of view in radians
  }
data GCFPar = GCFPar
  { gcfSize :: Int
  , gcfOver :: Int
  , gcfFile :: FilePath
  }

-- | Image dimensions for all facets together
gridImageWidth :: GridPar -> Int
gridImageWidth gp = gridWidth gp * gridFacets gp

gridImageHeight :: GridPar -> Int
gridImageHeight gp = gridHeight gp * gridFacets gp

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

type UDom = Domain Range -- ^ Domain used for the u dimension
type VDom = Domain Range -- ^ Domain used for the v dimension
type WDom = Domain Bins  -- ^ Domain used for the w dimension

type UVGRepr = RangeRepr (RangeRepr (HalideRepr Dim1 Double UVGrid))
uvgRepr :: UDom -> VDom -> UVGRepr
uvgRepr udom vdom =
  RangeRepr vdom $
  RangeRepr udom $
  halideRepr (dim1 dimCpx)

type UVGMarginRepr = MarginRepr (MarginRepr (HalideRepr Dim1 Double UVGrid))
uvgMarginRepr :: GCFPar -> UDom -> VDom -> UVGMarginRepr
uvgMarginRepr gcfp udom vdom =
  marginRepr vdom (gcfSize gcfp `div` 2) $
  marginRepr udom (gcfSize gcfp `div` 2) $
  halideRepr (dim1 dimCpx)

dimCpx :: Dim
dimCpx = (0, 2)

type FacetRepr = HalideRepr Dim2 Double Image
facetRepr :: GridPar -> ImageRepr
facetRepr gp = halideRepr $ dimY :. dimX :. Z
  where dimX = (0, fromIntegral $ gridWidth gp)
        dimY = (0, fromIntegral $ gridHeight gp)

type ImageRepr = HalideRepr Dim2 Double Image
imageRepr :: GridPar -> ImageRepr
imageRepr gp = halideRepr $ dimY :. dimX :. Z
  where dimX = (0, fromIntegral $ gridImageWidth gp)
        dimY = (0, fromIntegral $ gridImageHeight gp)

type PlanRepr = NoRepr Tag -- HalideRepr Dim0 Int32 Tag
planRepr :: PlanRepr
planRepr = NoRepr -- halideRepr dim0

-- | Raw visibilities: Dynamically sized list of visibility records
-- (see "dimVisFields").
type RawVisRepr = RangeRepr (HalideRepr Dim1 Double Vis)
rawVisRepr :: Domain Range -> RawVisRepr
rawVisRepr dom = RangeRepr dom $ halideRepr (dim1 dimVisFields)

type RotatedVisRepr = RegionRepr Range (RegionRepr Range (RangeRepr (HalideRepr Dim1 Double Vis)))
rotatedVisRepr :: Domain Range -> Domain Range -> Domain Range -> RotatedVisRepr
rotatedVisRepr ldom mdom tdom =
  RegionRepr ldom $ RegionRepr mdom $ RangeRepr tdom $
  halideRepr (dim1 dimVisFields)

type VisRepr = RegionRepr Range (RegionRepr Range (BinRepr (HalideRepr Dim1 Double Vis)))
visRepr :: UDom -> VDom -> WDom -> VisRepr
visRepr udom vdom wdom =
  RegionRepr udom $ RegionRepr vdom $ BinRepr wdom $
  halideRepr (dim1 dimVisFields)

-- | We have 5 visibility fields: u, v and w, Real, imag
dimVisFields :: Dim
dimVisFields = (0, 5)

type GCFsRepr = RegionRepr Bins (HalideRepr Dim4 Double GCFs)
gcfsRepr :: WDom -> GCFPar -> GCFsRepr
gcfsRepr wdom gcfp = RegionRepr wdom $ halideRepr $ dimOver :. dimSize :. dimSize :. dimCpx :. Z
  where dimOver = (0, fromIntegral $ gcfOver gcfp * gcfOver gcfp)
        dimSize = (0, fromIntegral $ gcfSize gcfp)
