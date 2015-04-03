{-# LANGUAGE
      TypeSynonymInstances
    , FlexibleInstances
    , DeriveGeneric
    , DeriveDataTypeable
  #-}

module OskarBinReader where

import OskarBinReaderFFI
import Foreign
import Foreign.C
import Foreign.Storable.Complex ()
import Text.Printf

import GHC.Generics (Generic)
import Data.Binary
import BinaryInstances ()
import Data.Typeable

data TaskData = TaskData {
    tdBaselines :: !Int
  , tdTimes     :: !Int
  , tdChannels  :: !Int
  , tdPoints    :: !Int
  , tdMaxx      :: !CDouble
  , tdWstep     :: !CDouble
  , tdVisibilies :: !(Ptr CxDouble)
  , tdUVWs :: !(Ptr CDouble)
  , tdMap  :: !(Ptr BlWMap)
} deriving (Generic, Typeable)

instance Binary TaskData

finalizeTaskData :: TaskData -> IO ()
finalizeTaskData td = do
  free $ tdMap td
  free $ tdUVWs td
  free $ tdVisibilies td

data SortType = NoSort | PlainSort | NormSort

-- FIXME: Add type layer to distingush between cloned and original data.
-- Another option is to attach finalizer to the data itself, but
--  that would require bothering with Binary instance.
mkSortedClone :: SortType -> TaskData -> IO TaskData
mkSortedClone st td = do
    newMap <- mallocArray n
    copyArray newMap (tdMap td) n
    sort st newMap (fromIntegral n)
    return td {tdMap = newMap}
  where
    n = tdBaselines td
    sort NoSort = \_ _ -> return ()
    sort PlainSort = sort_on_w
    sort NormSort = sort_on_abs_w

finalizeSortedClone :: TaskData -> IO ()
finalizeSortedClone td = free $ tdMap td

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
      visptr <- mallocArray (n * 4)
      uvwptr <- mallocArray (n * 3)
      mapptr <- mallocArray nb
      alloca $ \mptr ->
        allocaArray nb $ \mmptr -> do
          throwErr $ readAndReshuffle vptr visptr uvwptr mptr mmptr mapptr
          wstp <- wstep mptr
          mxx <- maxx mptr
          return $ TaskData nb (fi ntms) (fi nchs) n mxx wstp visptr uvwptr mapptr
