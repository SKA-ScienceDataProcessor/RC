{-# LANGUAGE
      ForeignFunctionInterface
    , CPP
  #-}

#include "OskarBinReader.h"

module OskarBinReaderFFI where

import Foreign
import Foreign.C
import Data.Complex

#let fic t = "foreign import ccall %s ", #t
#let opaq typ = "data %s\ninstance Storable %s where\n\tsizeOf _ = %ld\n\talignment _ = %ld\n\tpeek = error \"no peek for %s\"\n\tpoke = error \"no poke for %s\"\n", #typ, #typ, sizeof(typ), __alignof__(typ), #typ, #typ

#opaq BlWMap
#opaq Metrix
#opaq WMaxMin
#opaq VisData

numBaselines, numTimes, numChannels, numPoints :: Ptr VisData -> IO CInt
numBaselines = #peek VisData, num_baselines
numTimes     = #peek VisData, num_times
numChannels  = #peek VisData, num_channels
numPoints    = #peek VisData, num_points

#fic mkFromFile :: Ptr VisData -> CString -> IO CInt 
#fic freeBinHandler :: Ptr VisData -> IO () 

type CxDouble = Complex Double

#fic readAndReshuffle :: Ptr VisData -> Ptr CxDouble -> Ptr CDouble -> Ptr Metrix -> Ptr WMaxMin -> Ptr BlWMap -> IO CInt 
#fic sort_on_w :: Ptr BlWMap -> CInt -> IO () 
#fic sort_on_abs_w :: Ptr BlWMap -> CInt -> IO () 
