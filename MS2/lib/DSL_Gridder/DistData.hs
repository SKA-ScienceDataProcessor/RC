{-# LANGUAGE
    TupleSections
  #-}

module DistData where

import System.IO.MMap (
    Mode(..)
  , mmapWithFilePtr
  )
import Foreign.Ptr (Ptr)

data DistData =
    LocalDataSimple {
        ldBuf :: Ptr ()
      , ldLen :: Int
      }
  | RemoteDataSimple {
        rdPath :: FilePath
      , rdSize :: Maybe Int
      }

withDistData :: DistData -> (Ptr () -> Int -> IO a) -> IO a
withDistData (LocalDataSimple p n) f = f p n
withDistData (RemoteDataSimple fp mbsize) f =
    mmapWithFilePtr fp (mode mbsize) (range mbsize) (uncurry f)
  where
    mode Nothing = ReadOnly
    mode _ = ReadWriteEx
    range = fmap (0,)

withDistData2 :: DistData -> DistData -> (Ptr () -> Int -> Ptr () -> Int -> IO a) -> IO a
withDistData2 dd1 dd2 f =
  withDistData dd1 $ \p1 n1 ->
    withDistData dd2 $ \p2 n2 -> f p1 n1 p2 n2
