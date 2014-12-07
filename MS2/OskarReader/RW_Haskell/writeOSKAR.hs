{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (finally)
import System.IO.MMap (
    Mode(..)
  , mmapFilePtr
  , munmapFilePtr
  )
import Foreign.Ptr (plusPtr)
import Foreign.Storable (sizeOf)
import System.Environment (getArgs)
import System.FilePath (addExtension)
import Foreign.C.Types (CDouble)

import OskarFFI

writeOSKAR
    :: OVisData
    -> FilePath
    -> FilePath
    -> IO Status
writeOSKAR (OVD hdl ts_x_bl chans write_and_fin) path_amp path_uvw = do
    (ptru, rawsizeu, offsetu, _sizeu) <- mmapFilePtr path_uvw ReadWriteEx (Just (0, 3 * ts_x_bl_bytes))
    (ptra, rawsizea, offseta, _sizea) <- mmapFilePtr path_amp ReadWriteEx (Just (0, 8 * ts_x_bl_bytes * chans))
    finally
      (write_and_fin hdl (ptra `plusPtr` offseta) (ptru `plusPtr` offsetu))
      (munmapFilePtr ptru rawsizeu >> munmapFilePtr ptra rawsizea)
  where
    ts_x_bl_bytes = ts_x_bl * sizeOf (undefined :: CDouble)


main :: IO ()
main = do
  fname <- fmap head getArgs
  evd <- vis_allocate_and_read fname
  let fadd_ex = addExtension fname
  case evd of
    Left status -> putStrLn $ "Error: " ++ show status
    Right ovd -> writeOSKAR ovd (fadd_ex "amp") (fadd_ex "uvw") >>= putStrLn . ("Done with: " ++) . show
