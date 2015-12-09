{-# LANGUAGE DataKinds #-}

module Kernel.Facet where

import Data.Int

import Flow.Builder
import Flow.Domain
import Flow.Halide
import Flow.Kernel

import Kernel.Data

-- For FFI
import Data.Vector.HFixed.Class ()
import Flow.Halide.Types ()

-- | Visibility rotation
rotateKernel
  :: GridPar           -- ^ Configuration
  -> Double -> Double  -- ^ Input longitude / latitude (radians)
  -> Double -> Double  -- ^ Output longitude / latitude (radians)
  -> Bool -- ^ Do uv-projection aka image plane facetting? If yes,
          -- this will adjust UVW as well as the visibility.
  -> Domain Range -- ^ L domain
  -> Domain Range -- ^ M domain
  -> Domain Range -- ^ Visibility domain
  -> Flow Vis
  -> Kernel Vis
rotateKernel gp inLon inLat outLon outLat doRep ldom mdom tdom =
  -- Calculate start longitude/latitude and increments based on
  -- minimum L/M. This essentially means that we need to calculate the
  -- angle of the image centre from the pixel position of the top-left
  -- corner of the facet relative to the complete picture.
  let wdt = fromIntegral $ gridWidth gp
      hgt = fromIntegral $ gridHeight gp
      facets = fromIntegral $ gridFacets gp
      lonIncr = gridTheta gp / wdt / facets -- radians per pixel
      latIncr = gridTheta gp / hgt / facets -- radians per pixel
      lon0 = outLon - lonIncr * (fromIntegral $ (gridImageWidth gp `div` 2) - (gridWidth gp `div` 2))
      lat0 = outLat - latIncr * (fromIntegral $ (gridImageHeight gp `div` 2) - (gridHeight gp `div` 2))
  in halideKernel1 "rotateKernel" (rawVisRepr tdom)
                                  (rotatedVisRepr ldom mdom tdom) $
     kern_rotate `halideBind` inLon `halideBind` inLat
                 `halideBind` lon0 `halideBind` lat0
                 `halideBind` lonIncr `halideBind` latIncr
                 `halideBind` (if doRep then 1 else 0)
foreign import ccall unsafe kern_rotate
  :: (HalideBind Double (HalideBind Double
     (HalideBind Double (HalideBind Double
     (HalideBind Double (HalideBind Double
     (HalideBind Int32
     (HalideFun '[RawVisRepr] RotatedVisRepr))))))))

-- | Defacetting image initialisation
imageInitDetile :: GridPar -> Kernel Image
imageInitDetile gp = halideKernel0 "imageInit" (imageRepr gp) kern_image_init
foreign import ccall unsafe kern_image_init :: HalideFun '[] ImageRepr

-- | Grid de-tiling kernel. This simply copies tiled (and possibly
-- overlapped) tiles into a common UV-grid.
imageDefacet :: GridPar -> (Domain Range, Domain Range) -> Flow Image -> Flow Image -> Kernel Image
imageDefacet gp (ydom0, xdom0) =
  halideKernel1Write "imageDefacet" (RegionRepr ydom0 $ RegionRepr xdom0 $ facetRepr gp)
                                    (imageRepr gp) kern_defacet
foreign import ccall unsafe kern_defacet :: HalideFun '[RegionRepr Range (RegionRepr Range FacetRepr)] ImageRepr

