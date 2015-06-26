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
  , VisBaseline(..)
  , dumpVis
  , constVis
  , subtractVis
  , sortBaselines

    -- * GCF
  , GCFPar(..)
  , GCF(..)
  , GCFSet(..)

    -- * Common
  , UVW(..), Polar(..)
  , CleanPar(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Complex
import Data.Binary   (Binary)
import Data.List     (sortBy)
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Complex ( )
import GHC.Generics  (Generic)
import Text.Printf

import DNA.Channel.File

import Config
import Vector

-- | The @UVGrid@ contains gridded complex-valued visibilities in the frequency
-- domain. Conceptually, it is hermitian in nature - so @G(u,v) =
-- conj(G(-u,-v))@. This means that we are actually interested in the
-- upper half of it.
data UVGrid = UVGrid
  { uvgPar  :: GridPar
  , uvgPadding :: !Int  -- ^ Amount of padding before actual data
                        -- starts in "uvgData" (in "Complex Double" elements).
  , uvgData :: Vector (Complex Double)
  }
  deriving (Show,Typeable,Generic)
instance Binary UVGrid

-- | Images correspond to real-valued sky brightness data. They can be transformed
-- to and from the @UVGrid@ using a fourier transformation.
data Image = Image
  { imgPar :: GridPar   -- ^ Image data parameters. We use the same
                        -- format as the "UVGrid" here, except with
                        -- "Double" as our element type.
  , imgPadding :: !Int  -- ^ Amount of padding before actual data
                        -- starts in "imgData" (in "Double" element).
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
  { visMinW      :: !Double       -- ^ Minimum seen @w@ value
  , visMaxW      :: !Double       -- ^ Maximum seen @w@ value
  , visTimesteps :: !Int          -- ^ Number of timesteps (=visibilities) per baseline
  , visBaselines :: [VisBaseline] -- ^ Possibily sorted map of visibility data to baselines
  , visPositions :: Vector UVW    -- ^ Visibility positions in the (u,v,w) plane
  , visData      :: Vector (Complex Double) -- ^ Visibility data
  , visBinData   :: Vector ()  -- ^ Binning data. Kernels might use
                               -- this to traverse visibilities in an
                               -- optimised fashion.
  }
  deriving (Typeable,Generic)
instance Binary Vis

-- | Dump visibility information to "stdout"
dumpVis :: Vis -> IO ()
dumpVis v = do
    putStrLn $ concat [ "Visibilities -- "
                      , show (length (visBaselines v)), " baselines, "
                      , show (visTimesteps v), " timesteps, "
                      , show (vectorSize (visPositions v)), " positions, "
                      , show (vectorSize (visData v)), " visibilities"
                      , show (vectorSize (visBinData v)), " binning data"
                      ]
    posV <- dupCVector $ visPositions v
    visV <- dupCVector $ visData v
    forM_ (visBaselines v) $ \bl -> do
        putStrLn $ "Baseline @ " ++ show (vblOffset bl) ++ ":"
        forM_ [0..vblPoints bl-1] $ \p -> do
            pos <- peekVector posV (p + vblOffset bl)
            vr :+ vi <- peekVector visV (p + vblOffset bl)
            putStrLn $ printf " %8.02f / %8.02f / %8.02f: %6.03f%-+.03fi"
                              (uvwU pos) (uvwV pos) (uvwW pos) vr vi
    freeVector posV
    freeVector visV

-- | Visibilities are a list of correlator outputs from a dataset
-- concerning the same frequency band and polarisation.
data VisBaseline = VisBaseline
  { vblOffset    :: !Int     -- ^ Offset into the "visPositions" / "visData" vector
  , vblPoints    :: !Int     -- ^ Number of points
  , vblMinW      :: !Double  -- ^ Minimum @w@ value
  , vblMaxW      :: !Double  -- ^ Maximum @w@ value
  }
  deriving (Show,Typeable,Generic)
instance Binary VisBaseline

-- | Visibility position in the uvw coordinate system
data UVW = UVW
  { uvwU :: !Double
  , uvwV :: !Double
  , uvwW :: !Double
  }
  deriving (Show,Typeable,Generic)
instance Binary UVW
instance Storable UVW where
  sizeOf    _ = 3 * sizeOf (undefined :: Double)
  alignment _ = alignment (undefined :: Double)
  peek p = UVW <$> peekElemOff (castPtr p) 0
               <*> peekElemOff (castPtr p) 1
               <*> peekElemOff (castPtr p) 2
  poke p (UVW u v w) = do
     pokeElemOff (castPtr p) 0 u
     pokeElemOff (castPtr p) 1 v
     pokeElemOff (castPtr p) 2 w

-- | Generate a duplicate of the visibilities, setting all of them to
-- the given value. Useful for determining the response to 
constVis :: Complex Double -> Vis -> IO Vis
constVis = undefined

-- | Subtract two visibility sets from each other. They must be using
-- the same positions.
subtractVis :: Vis -> Vis -> IO Vis
subtractVis = undefined

-- | Sort baselines according to the given sorting functions
-- (e.g. `comparing vblMinW`)
sortBaselines :: (VisBaseline -> VisBaseline -> Ordering) -> Vis -> Vis
sortBaselines f v = v { visBaselines = sortBy f (visBaselines v) }

-- | A set of GCFs.
data GCFSet = GCFSet
  { gcfsPar :: GCFPar  -- ^ GCF parameterisiation
  , gcfs :: [GCF]      -- ^ The contained GCFs. Sorted ascending by @w@ value.
  , gcfTable :: Vector () -- ^ GCF lookup table. Used by the gridding algorithm to speed up access.
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
