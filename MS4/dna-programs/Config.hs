{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | All quasi-static configuration that is used to run the imaging
-- pipeline. This covers both data structures for configuration of the
-- top-level program as well as structures steering the behaviour of
-- individual actors.
module Config where

import Control.Applicative
import Control.Monad (mzero)

import Data.Aeson
import Data.Binary   (Binary)
import Data.Typeable (Typeable)

import GHC.Generics  (Generic)

import DNA.Channel.File

data OskarData

-- | Main run configuration. This contains all parameters we need for
-- runnin the imaging pipeline.
data Config = Config
  { cfgGridPar :: GridPar    -- ^ Grid parameters to use. Must be compatible with used kernels!
  , cfgGCFPar :: GCFPar      -- ^ GCF parameters to use.

  , cfgGCFKernel :: String   -- ^ The GCF kernel to use
  , cfgGridKernel :: String  -- ^ The gridding kernel to use
  , cfgDFTKernel :: String   -- ^ The fourier transformation kernel to use
  , cfgCleanKernel :: String -- ^ The cleaning kernel to use

  , cfgMajorLoops :: Int     -- ^ Maximum number of major loop iterations
  , cfgCleanPar :: CleanPar  -- ^ Parameters to the cleaning kernel
  }
  deriving (Show,Generic,Typeable)
instance Binary Config
instance FromJSON Config where
  parseJSON (Object v)
    = Config <$> v .: "grid" <*> v .: "gcf"
             <*> v .: "gcfkernel" <*> v .: "gridkernel"
             <*> v .: "dftkernel" <*> v .: "cleankernel"
             <*> v .: "loops" <*> v .: "clean"
  parseJSON _ = mzero

-- | Cleaning parameterisiation
data CleanPar = CleanPar
  { cleanIter :: !Int    -- ^ Maximum number of iterations for the minor loop
  , cleanGain :: !Double -- ^ "Loop gain" - fraction of brightest pixel removed in each iteration
  , cleanThreshold :: !Double -- ^ Threshold at which we should stop cleaning
  }
  deriving (Show,Typeable,Generic)
instance Binary CleanPar
instance FromJSON CleanPar where
  parseJSON (Object v)
    = CleanPar <$> v .: "iter" <*> v .: "gain"
               <*> v .: "threshold"
  parseJSON _ = mzero

-- | A data set, consisting of an Oskar file name and the frequency
-- channel & polarisation we are interested in.
data DataSet = DataSet
  { dsName :: String   -- ^ Name to use to refer to this data set
  , dsData :: FileChan OskarData -- ^ Oskar data to read
  , dsChannel :: Int   -- ^ Frequency channel to process
  , dsPolar :: Polar   -- ^ Polarisation to process
  , dsRepeats :: Int   -- ^ Number of times we should process this
                       -- data set to simulate a larger workload
  }
  deriving (Generic,Typeable)
instance Binary DataSet

-- | Grid parameters. This defines how big our grid is going to be,
-- and how it is going to be stored.
data GridPar = GridPar
  { gridWidth :: !Int  -- ^ Width of the uv-grid/image in pixels
  , gridHeight :: !Int -- ^ Neight of the uv-grid/image in pixels
  , gridPitch :: !Int  -- ^ Distance between rows in grid storage. Can
                       -- be larger than width if data is meant to be
                       -- padded.
  , gridTheta :: !Double  -- ^ Size of the field of view in radians
  }
  deriving (Show,Typeable,Generic,Eq)
instance Binary GridPar
instance FromJSON GridPar where
  parseJSON (Object v)
    = GridPar <$> v .: "width" <*> v .: "height"
              <*> v .: "pitch" <*> v .: "theta"
  parseJSON _ = mzero

-- | Size of the uv-grid in wavelengths / pixel size of a radian in
-- images
gridLambda :: GridPar -> Double
gridLambda gp = fromIntegral (gridWidth gp) / gridTheta gp

-- | Minimum number of rows we need to allocate for the grid in order
-- to run a complex-to-real FFT. This is generally less than the full
-- height.
gridHalfHeight :: GridPar -> Int
gridHalfHeight gp = (gridHeight gp `div` 2) + 1

-- | Minimum size required to allocate a full FFT-able grid, in
-- elements ("Complex Double")
gridHalfSize :: GridPar -> Int
gridHalfSize gp = gridHalfHeight gp * gridPitch gp

-- | Size of the grid in elements ("Complex Double") if we want to
-- allocate it in full
gridFullSize :: GridPar -> Int
gridFullSize gp = gridHeight gp * gridPitch gp

-- | Parameters going into GCF generation
data GCFPar = GCFPar
  { gcfpOver :: !Int      -- ^ Amount of oversampling. Note that
                          -- kernels might only work with one
                          -- oversampling value!
  , gcfpStepW :: !Double  -- ^ Size of the @w@ bins
  , gcfpGrowth :: !Int    -- ^ Size of the GCF depending on targeted
                          -- @w@ value (exact formula TBD).
  , gcfpMinSize :: !Int   -- ^ Minimum size for the GCF
  , gcfpMaxSize :: !Int   -- ^ Maximum size for the GCF
  }
  deriving (Show,Typeable,Generic)
instance Binary GCFPar
instance FromJSON GCFPar where
  parseJSON (Object v)
    = GCFPar <$> v .: "over" <*> v .: "stepw" <*> v .: "growth"
             <*> v .: "minsize" <*> v .: "maxsize"
  parseJSON _ = mzero

-- | Visibility polarisation
data Polar = XX | XY | YX | YY
           deriving (Show,Typeable,Generic,Enum,Bounded)
instance Binary Polar
instance FromJSON Polar where
  parseJSON (String "XX") = return XX
  parseJSON (String "XY") = return XY
  parseJSON (String "YX") = return YX
  parseJSON (String "YY") = return YY
  parseJSON _             = mzero
