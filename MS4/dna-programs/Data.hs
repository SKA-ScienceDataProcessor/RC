{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data
  ( -- * Grid
    GridPar(..)
  , UVGrid(..)
  , gridHalfHeight
  , gridHalfSize
  , gridFullSize

    -- * Image
  , Image(..)
  , imageSize
  , constImage
  , addImage
  , writeImage

    -- * Visibilities
  , Vis(..)
  , constVis
  , subtractVis

    -- * GCF
  , GCFPar(..)
  , GCF(..)
  , GCFSet(..)

    -- * Common
  , UVW(..), Polar(..)
  , CleanPar(..)
  ) where

import Data.Complex
import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)
import Foreign.Storable.Complex ( )

import DNA.Channel.File

import Vector


-- | Grid parameters. This defines how big our grid is going to be,
-- and how it is going to be stored.
data GridPar = GridPar
  { gridWidth :: !Int  -- ^ Width of the grid in pixels
  , gridHeight :: !Int -- ^ Neight of the grid in pixels
  , gridPitch :: !Int  -- ^ Distance between rows in grid storage. Can
                       -- be larger than width if data is meant to be
                       -- padded.
  }
  deriving (Show,Typeable,Generic)
instance Binary GridPar

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
  { gcfpStepW :: !Double -- ^ Size of the @w@ bins
  , gcfpMaxSize :: !Int  -- ^ Maximum size for the GCF (?)
  }
  deriving (Show,Typeable,Generic)
instance Binary GCFPar

-- | The @UVGrid@ contains gridded complex-valued visibilities in the frequency
-- domain. Conceptually, it is hermitian in nature - so @G(u,v) =
-- conj(G(-u,-v))@. This means that we are actually interested in the
-- upper half of it.
data UVGrid = UVGrid
  { uvgPar :: GridPar
  , uvgData :: Vector (Complex Double)
  }
  deriving (Show,Typeable,Generic)
instance Binary UVGrid

-- | Images correspond to real-valued sky brightness data. They can be transformed
-- to and from the @UVGrid@ using a fourier transformation.
data Image = Image
  { imgPar :: GridPar
  , imgData :: Vector Double
  }
  deriving (Show,Typeable,Generic)
instance Binary Image

-- | Returns the number of elements ("Double") required to store the image. Note
-- that for consistency we honor the pitch requirements of the grid.
imageSize :: GridPar -> Int
imageSize gp = gridHeight gp * gridPitch gp

-- | Create an image filled with the given value, allocated as a
-- simple C buffer.
constImage :: GridPar -> Double -> IO Image
constImage = undefined

-- | Add two images together. Assumed to consume both images.
addImage :: Image -> Image -> IO Image
addImage = undefined

-- | Write image to a file channel
writeImage :: Image -> FileChan Image -> String -> IO ()
writeImage = undefined

-- | Visibilities are a list of correlator outputs from a dataset
-- concerning the same frequency band and polarisation.
--
-- This data will already be prepared: We will have made sure that the
-- positions satisfy @v+gcf_size/2 >= 0@. Furthermore, the
-- visibilities will be rotated according to a target position in the
-- centre of the observed field.
data Vis = Vis
  { visPositions :: Vector UVW -- ^ Visibility positions in the (u,v,w) plane
  , visMinW      :: Double     -- ^ Minimum seen @w@ value
  , visMaxW      :: Double     -- ^ Maximum seen @w@ value
  , visData      :: Vector (Complex Double) -- ^ Visibility data
  , visBinData   :: Vector ()  -- ^ Binning data. Kernels might use
                               -- this to traverse visibilities in an
                               -- optimised fashion.
  }
  deriving (Show,Typeable,Generic)
instance Binary Vis

-- | Visibility polarisation
data Polar = XX | YY | XY | YX
           deriving (Show,Typeable,Generic)
instance Binary Polar

-- | Visibility position in the uvw coordinate system
data UVW = UVW
  { uvwU :: !Double
  , uvwV :: !Double
  , uvwW :: !Double
  }
  deriving (Show,Typeable,Generic)
instance Binary UVW

-- | Generate a duplicate of the visibilities, setting all of them to
-- the given value. Useful for determining the response to 
constVis :: Complex Double -> Vis -> IO Vis
constVis = undefined

-- | Subtract two visibility sets from each other. They must be using
-- the same positions.
subtractVis :: Vis -> Vis -> IO Vis
subtractVis = undefined

-- | A set of GCFs.
data GCFSet = GCFSet
  { gcfsPar :: GCFPar  -- ^ GCF parameterisiation
  , gcfs :: [GCF]      -- ^ The contained GCFs. Sorted ascending by @w@ value.
  }
  deriving (Show,Typeable,Generic)
instance Binary GCFSet

-- | A grid convolution function, used for gridding visibilities
-- to the @UVGrid@. It is valid in a given range of @w@-values.
-- The size will also depend significantly on the @w@ value.
data GCF = GCF
  { gcfMinW :: !Double  -- ^ Low @w@ value it was generated for
  , gcfMaxW :: !Double  -- ^ High @w@ value is was generated for
  , gcfSize :: !Int     -- ^ Width and height of the convolution function in pixels
  , gcfData :: Vector (Complex Double)
                        -- ^ Convolution matrix data
  }
  deriving (Show,Typeable,Generic)
instance Binary GCF

-- | Cleaning parameterisiation
data CleanPar = CleanPar
  { cleanIter :: !Int    -- ^ Maximum number of iterations for the minor loop
  , cleanGain :: !Double -- ^ Model gain per source removed
  , cleanThreshold :: !Double -- ^ Threshold at which we should stop cleaning
  }
  deriving (Show,Typeable,Generic)
instance Binary CleanPar
