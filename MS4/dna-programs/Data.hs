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
  , freeImage
  , constImage
  , addImage
  , readImage
  , writeImage

    -- * Visibilities
  , Vis(..)
  , VisBaseline(..)
  , freeVis
  , dumpVis
  , constVis
  , sortBaselines
  , baselineMinWPlane

    -- * GCF
  , GCFPar(..)
  , GCF(..)
  , GCFSet(..)
  , freeGCFSet
  , findGCF

    -- * Common
  , UVW(..), Polar(..)
  , CleanPar(..)
  ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.Complex
import Data.Binary   (Binary)
import Data.List     (sortBy, find)
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Complex ( )
import GHC.Generics  (Generic)
import System.IO     (IOMode(..))
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

-- | Free data associated with an image
freeImage :: Image -> IO ()
freeImage img = freeVector (imgData img)

-- | Create an image filled with the given value, allocated as a
-- simple C buffer.
constImage :: GridPar -> Double -> IO Image
constImage gp v = do
   img <- allocCVector (imageSize gp)
   forM_ [0..imageSize gp-1] $ \i -> pokeVector img i v
   return $ Image gp 0 img

-- | Add two images together. Assumed to consume both images.
addImage :: Image -> Image -> IO Image
addImage img1 img2 = do

   -- Check image data layout
   when (imgPar img1 /= imgPar img2) $
     fail $ "addImage: Got different image parameters: " ++ show (imgPar img1) ++ " vs " ++ show (imgPar img2)

   -- Convert both to C vectors
   img1' <- toCVector (imgData img1)
   img2' <- toCVector (imgData img2)
   forM_ [0..imageSize (imgPar img1)-1] $ \i -> do
     v1 <- peekVector img1' i
     v2 <- peekVector img2' i
     pokeVector img1' i (v1 + v2)
   freeVector img2'
   return img1{imgData=img1'}

-- | Read image from a file channel
readImage :: Image -> FileChan Image -> String -> IO ()
readImage img chan file =
  withFileChan chan file WriteMode $ \h ->
    BS.hPut h =<< unsafeToByteString (imgData img)

-- | Write image to a file channel
writeImage :: Image -> FileChan Image -> String -> IO ()
writeImage img chan file =
  withFileChan chan file WriteMode $ \h ->
    BS.hPut h =<< unsafeToByteString (imgData img)

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
  , visPregridded :: Vector ()    -- ^ Visibilitiy positions, prepared for gridding
  , visBinData   :: Vector ()  -- ^ Binning data. Kernels might use
                               -- this to traverse visibilities in an
                               -- optimised fashion.
  }
  deriving (Typeable,Generic)
instance Binary Vis

-- | Free data associated with a visibility set
freeVis :: Vis -> IO ()
freeVis vis = do
    freeVector (visPositions vis)
    freeVector (visData vis)
    freeVector (visBinData vis)
    freeVector (visPregridded vis)

-- | Dump visibility information to "stdout"
dumpVis :: Vis -> IO ()
dumpVis v = do
    putStrLn $ concat [ "Visibilities -- "
                      , show (length (visBaselines v)), " baselines, "
                      , show (visTimesteps v), " timesteps, "
                      , show (vectorSize (visPositions v)), " positions, "
                      , show (vectorSize (visData v)), " visibilities, "
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
-- the given value.
constVis :: Complex Double -> Vis -> IO Vis
constVis val vis = do
  pos' <- dupCVector (visPositions vis)
  data' <- dupCVector (visData vis)
  forM_ [0..vectorSize data'] $ \i ->
     pokeVector data' i val
  return vis { visPositions = pos'
             , visData = data'
             , visBinData = nullVector
             }

-- | Sort baselines according to the given sorting functions
-- (e.g. `comparing vblMinW`)
sortBaselines :: (VisBaseline -> VisBaseline -> Ordering) -> Vis -> Vis
sortBaselines f v = v { visBaselines = sortBy f (visBaselines v) }

-- | Get minimum W-plane covered by a baseline
baselineMinWPlane :: Double -> VisBaseline -> Int
baselineMinWPlane wstep bl
  | vblMinW bl > 0 = round (vblMinW bl / wstep)
  | vblMaxW bl < 0 = round (-vblMaxW bl / wstep)
  | otherwise      = 0

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
  { gcfMidW :: !Double  -- ^ Target @w@ value it was generated
                        -- for. Might not be the middle of "gcfMinW"
                        -- and "gcfMax" if that is benificial.
  , gcfMinW :: !Double  -- ^ Low @w@ value it was generated for
  , gcfMaxW :: !Double  -- ^ High @w@ value is was generated for
  , gcfSize :: !Int     -- ^ Width and height of the convolution function in pixels
  , gcfData :: Vector (Complex Double)
                        -- ^ Convolution matrix data
  }
  deriving (Show,Typeable,Generic)
instance Binary GCF

-- | Free data associated with a GCF set
freeGCFSet :: GCFSet -> IO ()
freeGCFSet gcfSet = do
  freeVector $ gcfTable gcfSet
  forM_ (gcfs gcfSet) $ \gcf ->
    freeVector (gcfData gcf)

-- | Find GCF appropriate for given @w@-value
findGCF :: GCFSet -> Double -> Maybe GCF
findGCF gcfSet w = find inRange $ gcfs gcfSet
  where inRange gcf = w >= gcfMinW gcf && w < gcfMaxW gcf
