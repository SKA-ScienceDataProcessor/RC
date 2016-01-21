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
  :: Config -- ^ Configuration
  -> LMDom  -- ^ Image coordinate domains
  -> TDom   -- ^ Visibility indexdomain
  -> Flow Vis
  -> Kernel Vis
rotateKernel cfg lmdom tdom =
  let gp = cfgGrid cfg
      wdt = fromIntegral $ gridWidth gp
      hgt = fromIntegral $ gridHeight gp
      -- For now we we only reproject for facets, so the base input
      -- and output positions are the same
      inLon = cfgLong cfg; inLat = cfgLat cfg
      outLon = cfgLong cfg; outLat = cfgLat cfg
      doRep = False
      -- Calculate start longitude/latitude and increments based on
      -- minimum L/M. This essentially means that we need to calculate the
      -- angle of the image centre from the pixel position of the top-left
      -- corner of the facet relative to the complete picture.
      facets = fromIntegral $ gridFacets gp
      lonIncr = gridTheta gp / wdt / facets -- radians per pixel
      latIncr = gridTheta gp / hgt / facets -- radians per pixel
      lon0 = outLon - lonIncr * (fromIntegral $ (gridImageWidth gp `div` 2) - (gridWidth gp `div` 2))
      lat0 = outLat - latIncr * (fromIntegral $ (gridImageHeight gp `div` 2) - (gridHeight gp `div` 2))
  in halideKernel1 "rotateKernel" (rawVisRepr tdom)
                                  (rotatedVisRepr lmdom tdom) $
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
imageInit :: GridPar -> Kernel Image
imageInit gp = halideKernel0 "imageInit" (imageRepr gp) kern_image_init
foreign import ccall unsafe kern_image_init :: HalideFun '[] ImageRepr

-- | Grid de-tiling kernel. This simply copies  tiles into a common UV-grid.
imageDefacet :: GridPar -> LMDom -> Flow Image -> Flow Image -> Kernel Image
imageDefacet gp (ldom, mdom) =
  halideKernel1Write "imageDefacet" (RegionRepr ldom $ RegionRepr mdom $ facetRepr gp)
                                    (imageRepr gp) kern_defacet
foreign import ccall unsafe kern_defacet :: HalideFun '[RegionRepr Range (RegionRepr Range FacetRepr)] ImageRepr

imageSum :: GridPar -> DDom -> DDom -> Flow Image -> Flow Image -> Kernel Image
imageSum gp ddom0 ddom1 =
  halideKernel1Write "imageSum" (RegionRepr ddom0 $ imageRepr gp)
                                (RegionRepr ddom1 $ imageRepr gp) kern_image_sum
foreign import ccall unsafe kern_image_sum :: HalideFun '[RegionRepr Bins ImageRepr] (RegionRepr Bins ImageRepr)
