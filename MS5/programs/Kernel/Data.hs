{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

-- | Data representation definitions
module Kernel.Data where

import Data.Int
import Data.Typeable

import Flow.Kernel
import Flow.Domain

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

type UVGridRepr = HalideRepr Dim2 Double UVGrid
uvgRepr :: GridPar -> UVGridRepr
uvgRepr gp = HalideRepr $ dim2 0 wdt 0 hgt
  where wdt = fromIntegral $ 2 * gridWidth gp
        hgt = fromIntegral $ gridHeight gp

type ImageRepr = HalideRepr Dim2 Double Image
imageRepr :: GridPar -> ImageRepr
imageRepr gp = HalideRepr $ dim2 0 wdt 0 hgt
  where wdt = fromIntegral $ gridWidth gp
        hgt = fromIntegral $ gridHeight gp

type PlanRepr = HalideRepr Dim0 Int32 Tag
planRepr :: PlanRepr
planRepr = HalideRepr dim0

type RawVisRepr = DynHalideRepr Double Vis
rawVisRepr :: DomainHandle Range -> RawVisRepr
rawVisRepr dh = DynHalideRepr dh

type VisRepr = DynHalideRepr Double Vis
visRepr :: DomainHandle Range -> RawVisRepr
visRepr dh = DynHalideRepr dh

type GCFsRepr = HalideRepr Dim2 Double GCFs
gcfsRepr :: GCFPar -> GCFsRepr
gcfsRepr gcfp = HalideRepr $ dim2 0 count 0 (size * size * 2)
  where count = fromIntegral $ gcfOver gcfp * gcfOver gcfp
        size = fromIntegral $ gcfSize gcfp
