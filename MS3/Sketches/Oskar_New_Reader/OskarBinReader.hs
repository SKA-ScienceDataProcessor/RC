{-# LANGUAGE
      TypeSynonymInstances
    , FlexibleInstances
  #-}


module OskarBinReader where

import OskarBinReaderFFI
import Foreign
import Foreign.C
import Foreign.Storable.Complex ()
import Text.Printf

data TaskData = TaskData {
    tdBaselines :: !Int
  , tdTimes     :: !Int
  , tdChannels  :: !Int
  , tdPoints    :: !Int
  , tdWstep     :: !CDouble
  , tdVisibilies :: !(Ptr CxDouble)
  , tdUVWs :: !(Ptr CDouble)
  , tdMap  :: !(Ptr BlWMap)
}

finalizeTaskData :: TaskData -> IO ()
finalizeTaskData td = do
  free $ tdMap td
  free $ tdUVWs td
  free $ tdVisibilies td

readOskarData :: String -> IO TaskData
readOskarData fname = withCString fname doRead
  where
    fi = fromIntegral
    throwErr = throwIf_ (/= 0) (\n -> printf "While trying to read binary file %s : %d" fname $ fi n)
    doRead namep = alloca $ \vptr -> do
      throwErr $ mkFromFile vptr namep
      nbls <- numBaselines vptr
      ntms <- numTimes    vptr
      nchs <- numChannels vptr
      -- it is in fact a product of previous three
      npts <- numPoints vptr
      let
        n = fi npts
        nb = fi nbls
        visSize = n * sizeOf (undefined :: CxDouble) * 4
        uvwSize = n * sizeOf (undefined :: CDouble) * 3
        mapSize = nb * sizeOf (undefined :: BlWMap)
      visptr <- mallocBytes visSize
      uvwptr <- mallocBytes uvwSize
      mapptr <- mallocBytes mapSize
      alloca $ \mptr ->
        allocaBytes (nb * sizeOf (undefined :: WMaxMin)) $ \mmptr -> do
          throwErr $ readAndReshuffle vptr visptr uvwptr mptr mmptr mapptr
          wstp <- wstep mptr
          return $ TaskData nb (fi ntms) (fi nchs) n wstp visptr uvwptr mapptr
