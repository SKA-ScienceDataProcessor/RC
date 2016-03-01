{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

-- | Data representation definitions
module Kernel.Data
  ( -- * Configuration
    OskarInput(..), Config(..), GridPar(..), GCFPar(..), CleanPar(..)
  , defaultConfig
  , gridImageWidth, gridImageHeight, gridScale, gridXY2UV
  -- * Data tags
  , Index, Tag, Vis, UVGrid, FullUVGrid, Image, Cleaned, GCFs
  -- * Data representations
  , DDom, TDom, UDom, VDom, WDom, UVDom, LDom, MDom, LMDom
  , IndexRepr, UVGRepr, UVGMarginRepr, FacetRepr, ImageRepr, FullUVGRepr, PlanRepr, GCFsRepr
  , indexRepr, uvgRepr, uvgMarginRepr, facetRepr, imageRepr, fullUVGRepr, planRepr, gcfsRepr
  -- * Visibility data representations
  , RawVisRepr, RotatedVisRepr, VisRepr
  , rawVisRepr, rotatedVisRepr, visRepr
  ) where

import Data.Typeable
import Data.Int ( Int32 )

import Flow.Halide
import Flow.Domain
import Flow.Kernel

import Kernel.Config

-- Data tags
data Index -- ^ Data set index
data Tag -- ^ Initialisation (e.g. FFT plans)
data Vis -- ^ Visibilities (File name to OSKAR / raw visibilities / binned ...)
data UVGrid -- ^ UV grid
data FullUVGrid -- ^ Full UV grid
data Image -- ^ Image
data Cleaned -- ^ Result from cleaning
data GCFs -- ^ A set of GCFs

deriving instance Typeable Tag
deriving instance Typeable Vis
deriving instance Typeable UVGrid
deriving instance Typeable FullUVGrid
deriving instance Typeable Image
deriving instance Typeable GCFs

type DDom = Domain Bins -- ^ Domain used for indexing data sets
type TDom = Domain Range -- ^ Domain used for indexing visibilities
type UDom = Domain Range -- ^ Domain used for the u grid dimension
type VDom = Domain Range -- ^ Domain used for the v grid dimension
type WDom = Domain Bins  -- ^ Domain used for the w grid pseudo-dimension
type UVDom = (UDom, VDom) -- ^ Domain used for the (u,v) grid dimensions
type LDom = Domain Range -- ^ Domain used for the l image dimension
type MDom = Domain Range -- ^ Domain used for the m image dimension
type LMDom = (LDom, MDom) -- ^ Domain used for the (l,m) image dimensions

type IndexRepr = BinRepr (HalideRepr Dim0 Int32 Index)
indexRepr :: DDom -> IndexRepr
indexRepr ddom = BinRepr ddom $ halideRepr dim0

type UVGRepr = RangeRepr (RangeRepr (HalideRepr Dim1 Double UVGrid))
uvgRepr :: UVDom -> UVGRepr
uvgRepr (udom, vdom) =
  RangeRepr vdom $
  RangeRepr udom $
  halideRepr (dim1 dimCpx)

type UVGMarginRepr = MarginRepr (MarginRepr (HalideRepr Dim1 Double UVGrid))
uvgMarginRepr :: GCFPar -> UVDom -> UVGMarginRepr
uvgMarginRepr gcfp (udom, vdom) =
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

type FullUVGRepr = HalideRepr Dim3 Double FullUVGrid
fullUVGRepr :: GridPar -> FullUVGRepr
fullUVGRepr gp = halideRepr $ dimY :. dimX :. dimCpx :. Z
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
rotatedVisRepr :: LMDom -> TDom -> RotatedVisRepr
rotatedVisRepr (ldom, mdom) tdom =
  RegionRepr ldom $ RegionRepr mdom $ RangeRepr tdom $
  halideRepr (dim1 dimVisFields)

type VisRepr = RegionRepr Range (RegionRepr Range (BinRepr (HalideRepr Dim1 Double Vis)))
visRepr :: UVDom -> WDom -> VisRepr
visRepr (udom, vdom) wdom =
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
