{-# LANGUAGE
      TypeSynonymInstances
    , FlexibleInstances
    , DeriveGeneric
    , DeriveDataTypeable
  #-}

module OskarBinReader
  ( TaskData(..)
  , SortType(..)
  , tdVisibilitiesSize
  , tdUVWSize
  , finalizeTaskData
  , mkSortedClone
  , finalizeSortedClone
  , readOskarData
  , writeTaskData
  , readTaskData
  , readTaskDataHeader
  ) where

import OskarBinReaderFFI
import Foreign
import Foreign.C
import Foreign.Storable.Complex ()
import Text.Printf
import System.IO
import System.FilePath
import Control.Applicative ( (<$>) )

import GHC.Generics (Generic)
import Data.Binary
import BinaryInstances ()
import Data.Typeable

import Utils

data TaskData = TaskData {
    tdBaselines :: !Int
  , tdTimes     :: !Int
  , tdChannels  :: !Int
  , tdPoints    :: !Int
  , tdMaxx      :: !CDouble
  , tdWstep     :: !CDouble
  , tdComplexity :: !Int
  , tdVisibilies :: !(Ptr CxDouble)
  , tdUVWs :: !(Ptr CDouble)
  , tdMap  :: !(Ptr BlWMap)
} deriving (Generic, Typeable)

instance Binary TaskData

tdVisibilitiesSize :: TaskData -> Int
tdVisibilitiesSize td = tdPoints td * 8 * sizeOf (undefined :: CDouble)

tdUVWSize :: TaskData -> Int
tdUVWSize td = tdPoints td * 3 * sizeOf (undefined :: CDouble)

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
      visptr <- alignedMallocArray (n * 4) 32
      uvwptr <- mallocArray (n * 3)
      mapptr <- mallocArray nb
      alloca $ \mptr ->
        allocaArray nb $ \mmptr -> do
          throwErr $ readAndReshuffle vptr visptr uvwptr mptr mmptr mapptr
          freeBinHandler vptr
          wstp <- wstep mptr
          mxx <- maxx mptr
          pcount <- fromIntegral <$> count_points mapptr (fromIntegral nb)
          return $ TaskData nb (fi ntms) (fi nchs) n mxx wstp pcount visptr uvwptr mapptr


-- Our Binary instance is unrelated to deep serializing.
-- Here is deep from/to disk marchalling.
writeTaskData :: String -> TaskData -> IO ()
writeTaskData namespace (TaskData nb nt nc np mxx wstp _ visptr uvwptr mapptr) =
  allocaArray 4 $ \ip ->
    allocaArray 2 $ \cdp -> do
      let
        pokei = poke . advancePtr ip
        pokec = poke . advancePtr cdp
      pokei 0 nb
      pokei 1 nt
      pokei 2 nc
      pokei 3 np
      pokec 0 mxx
      pokec 1 wstp
      let
        writeb handle = do
          let hput = hPutBuf handle
          hput ip (4 * sizeOf (undefined :: Int))
          hput cdp (2 * cdb_siz)
          hput visptr (np * 8 * cdb_siz)
          hput uvwptr (np * 3 * cdb_siz)
          hput mapptr (nb * sizeOf (undefined :: BlWMap))
      withBinaryFile (namespace </> "taskdata.dat") WriteMode writeb
  where
    cdb_siz = sizeOf (undefined :: CDouble)

readTaskDataHeader :: String -> IO TaskData
readTaskDataHeader = readTaskDataGen True

readTaskData :: String -> IO TaskData
readTaskData = readTaskDataGen False

readTaskDataGen :: Bool -> String -> IO TaskData
readTaskDataGen headerOnly namespace =
  allocaArray 4 $ \ip ->
    allocaArray 2 $ \cdp ->
      withBinaryFile (namespace </> "taskdata.dat") ReadMode $ \handle -> do
        let hget p siz = throwIf_ (/= siz)
                           (\n -> printf "While reading taskdata from %s got %d instead of %d" namespace n siz)
                           (hGetBuf handle p siz)
        hget ip (4 * sizeOf (undefined :: Int))
        hget cdp (2 * cdb_siz)
        let
          peeki = peek . advancePtr ip
          peekc = peek . advancePtr cdp
        nb   <- peeki 0
        nt   <- peeki 1
        nc   <- peeki 2
        np   <- peeki 3
        mxx  <- peekc 0
        wstp <- peekc 1
        let header = TaskData nb nt nc np mxx wstp 0 nullPtr nullPtr nullPtr
        if headerOnly then return header else do
          visptr <- alignedMallocArray (np * 4) 32
          uvwptr <- mallocArray (np * 3)
          mapptr <- mallocArray nb
          hget visptr (tdVisibilitiesSize header)
          hget uvwptr (tdUVWSize header)
          hget mapptr (nb * sizeOf (undefined :: BlWMap))
          pcount <- fromIntegral <$> count_points mapptr (fromIntegral nb)
          return header { tdComplexity = pcount
                        , tdVisibilies = visptr
                        , tdUVWs = uvwptr
                        , tdMap = mapptr
                        }
  where
    cdb_siz = sizeOf (undefined :: CDouble)
