{-# LANGUAGE
      ForeignFunctionInterface
    , CPP
    , DeriveDataTypeable
    , DeriveGeneric
  #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

#include "OskarBinReader.h"

module OskarBinReaderFFI where

import Data.Complex
import Data.Typeable (Typeable)
import Data.Binary (Binary)
import Foreign
import Foreign.C
import GHC.Generics (Generic)

#let fic t = "foreign import ccall %s ", #t
#let opaq typ = "data %s\ninstance Storable %s where\n\tsizeOf _ = %ld\n\talignment _ = %ld\n\tpeek = error \"no peek for %s\"\n\tpoke = error \"no poke for %s\"\n", #typ, #typ, sizeof(typ), __alignof__(typ), #typ, #typ

data Metrix = Metrix
  { mtxMaxU :: !Double
  , mtxMaxV :: !Double
  , mtxMaxW :: !Double
  , mtxMinU :: !Double
  , mtxMinV :: !Double
  , mtxMinW :: !Double
  }
  deriving (Typeable,Generic)
instance Binary Metrix
instance Storable Metrix where
  sizeOf _ = #size Metrix
  alignment _ = alignment (undefined :: Double)
  peek p = Metrix <$> (#peek Metrix, maxu) p <*> (#peek Metrix, maxv) p <*> (#peek Metrix, maxw) p
                  <*> (#peek Metrix, minu) p <*> (#peek Metrix, minv) p <*> (#peek Metrix, minw) p
  poke p (Metrix a b c d e f)
    =  (#poke Metrix, maxu) p a >> (#poke Metrix, maxv) p b >> (#poke Metrix, maxw) p c
    >> (#poke Metrix, minu) p d >> (#poke Metrix, minv) p e >> (#poke Metrix, minw) p f

data WMaxMin = WMaxMin
  { mmMaxW :: !Double
  , mmMinW :: !Double
  }
  deriving (Typeable,Generic)
instance Binary WMaxMin
instance Storable WMaxMin where
  sizeOf _ = #size WMaxMin
  alignment _ = alignment (undefined :: Double)
  peek p = WMaxMin <$> (#peek WMaxMin, maxw) p <*> (#peek WMaxMin, minw) p
  poke p (WMaxMin a b) = (#poke WMaxMin, maxw) p a >> (#poke WMaxMin, minw) p b

#opaq VisData

numBaselines, numTimes, numChannels, numPoints :: Ptr VisData -> IO CInt
numBaselines = #peek VisData, num_baselines
numTimes     = #peek VisData, num_times
numChannels  = #peek VisData, num_channels
numPoints    = #peek VisData, num_points

#fic mkFromFile :: Ptr VisData -> CString -> IO CInt
#fic freeBinHandler :: Ptr VisData -> IO ()

type CxDouble = Complex Double

#fic readAndReshuffle :: Ptr VisData -> Ptr CxDouble -> Ptr CDouble -> Ptr Metrix -> Ptr WMaxMin -> IO CInt
