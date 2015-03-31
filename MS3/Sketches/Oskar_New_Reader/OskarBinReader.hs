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
  , tdVisibilies :: !(ForeignPtr CxDouble)
  , tdUVWs :: !(ForeignPtr CDouble)
  , tdMap  :: !(ForeignPtr BlWMap)
}

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
      visFptr <- mallocForeignPtrBytes visSize
      uvwFptr <- mallocForeignPtrBytes uvwSize
      mapFptr <- mallocForeignPtrBytes mapSize
      alloca $ \mptr ->
        allocaBytes (nb * sizeOf (undefined :: WMaxMin)) $ \mmptr ->
          withForeignPtr visFptr $ \visptr ->
            withForeignPtr uvwFptr $ \uwvptr ->
              withForeignPtr mapFptr $ \mapptr -> do
                throwErr $ readAndReshuffle vptr visptr uwvptr mptr mmptr mapptr
                return $ TaskData nb (fi ntms) (fi nchs) n visFptr uvwFptr mapFptr
