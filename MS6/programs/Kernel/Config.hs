{-# LANGUAGE OverloadedStrings #-}

module Kernel.Config where

import Control.Applicative

import Data.Yaml

data Config = Config
  { cfgInput  :: [OskarInput] -- ^ Input Oskar files
  , cfgPoints :: Int      -- ^ Number of points to read from Oskar file
  , cfgNodes  :: Int      -- ^ Number of data sets to process in parallel
  , cfgLoops  :: Int      -- ^ Number of major loops to run
  , cfgLong   :: Double   -- ^ Phase centre longitude
  , cfgLat    :: Double   -- ^ Phase centre latitude
  , cfgOutput :: FilePath -- ^ File name for the output image
  , cfgGrid   :: GridPar
  , cfgGCF    :: GCFPar
  , cfgClean  :: CleanPar
  }
instance FromJSON Config where
  parseJSON (Object v)
    = Config <$> v .: "input"
             <*> v .: "points"
             <*> v .: "nodes"
             <*> (v .: "loops" <|> return (cfgLoops defaultConfig))
             <*> (v .: "long" <|> return (cfgLong defaultConfig))
             <*> (v .: "lat" <|> return (cfgLat defaultConfig))
             <*> v .: "output"
             <*> (v .: "grid" <|> return (cfgGrid defaultConfig))
             <*> (v .: "gcf" <|> return (cfgGCF defaultConfig))
             <*> (v .: "clean" <|> return (cfgClean defaultConfig))
  parseJSON v = mempty

data OskarInput = OskarInput
  { oskarFile   :: FilePath -- ^ Oskar file
  , oskarWeight :: Double   -- ^ Complexity
  , oskarRepeat :: Int      -- ^ Repeats
  }
instance FromJSON OskarInput where
  parseJSON (Object v)
    = OskarInput <$> v .: "file" <*> v .: "weight"
                 <*> (v .: "repeats" <|> return 1)
  parseJSON v = mempty

data GridPar = GridPar
  { gridWidth :: !Int  -- ^ Width of the uv-grid/image in pixels
  , gridHeight :: !Int -- ^ Neight of the uv-grid/image in pixels
  , gridPitch :: !Int  -- ^ Distance between rows in grid storage. Can
                       -- be larger than width if data is meant to be
                       -- padded.
  , gridTheta :: !Double  -- ^ Size of the field of view in radians

  , gridTiles  :: !Int -- ^ Number of tiles in U and V domains
  , gridFacets :: !Int -- ^ Number of facets in L and M domains
  , gridBins   :: !Int -- ^ Number of bins in W domain
  }
instance FromJSON GridPar where
  parseJSON (Object v)
    = GridPar <$> v .: "width" <*> v .: "height"
              <*> v .: "pitch"
              <*> v .: "theta"
              <*> (v .: "uv-tiles" <|> return 1)
              <*> (v .: "lm-facets" <|> return 1)
              <*> (v .: "w-bins" <|> return 1)
  parseJSON v = mempty

data GCFPar = GCFPar
  { gcfSize :: Int
  , gcfOver :: Int
  , gcfFile :: FilePath
  }
instance FromJSON GCFPar where
  parseJSON (Object v)
    = GCFPar <$> v .: "size" <*> v .: "over"
             <*> v .: "file"
  parseJSON v = mempty

data CleanPar = CleanPar
  { cleanGain      :: Double
  , cleanThreshold :: Double
  , cleanCycles    :: Int
  }
instance FromJSON CleanPar where
  parseJSON (Object v)
    = CleanPar <$> v .: "gain" <*> v .: "threshold"
               <*> v .: "cycles"
  parseJSON v = mempty

-- | Default configuration. Gets overridden by the actual
-- implementations where paramters actually matter.
defaultConfig :: Config
defaultConfig = Config
  { cfgInput  = []
  , cfgPoints = 32131
  , cfgNodes  = 0
  , cfgLoops  = 1
  , cfgLong   = 72.1 / 180 * pi -- mostly arbitrary, and probably wrong in some way
  , cfgLat    = 42.6 / 180 * pi -- ditto
  , cfgOutput = ""
  , cfgGrid   = GridPar 0 0 0 0 1 1 1
  , cfgGCF    = GCFPar 0 8 ""
  , cfgClean  = CleanPar 0 0 0
  }

-- | Image dimensions for all facets together
gridImageWidth :: GridPar -> Int
gridImageWidth gp = gridWidth gp * gridFacets gp

gridImageHeight :: GridPar -> Int
gridImageHeight gp = gridHeight gp * gridFacets gp

-- | Grid scale: FoV size in radians of one facet
gridScale :: GridPar -> Double
gridScale gp = gridTheta gp / fromIntegral (gridFacets gp)

-- | Convert a grid position into an u/v coordinate
gridXY2UV :: GridPar -> Int -> Double
gridXY2UV gp z = fromIntegral (z - gridHeight gp `div` 2) / gridScale gp
