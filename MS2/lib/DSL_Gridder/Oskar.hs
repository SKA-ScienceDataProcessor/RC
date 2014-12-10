{-# LANGUAGE
    MagicHash
  , UnliftedFFITypes
  , UnboxedTuples
  , GHCForeignImportPrim
  , GeneralizedNewtypeDeriving
  -- , DeriveDataTypeable
  , AutoDeriveTypeable
  #-}

module Oskar where

import Control.Exception (finally)
import System.IO.MMap (
    Mode(..)
  , mmapFilePtr
  , munmapFilePtr
  )
import GHC.Ptr (
    Ptr(..)
  , FunPtr(..)
  , plusPtr
  )
import GHC.Types(
    IO(..)
  , Int(I#)
  )
import GHC.Prim (
    Addr#
  , State#
  , RealWorld
  , Int#
  )
import Data.Typeable(Typeable)
import Data.Binary (Binary)
import Foreign.Storable (sizeOf)
import Foreign.C.Types (CDouble)
import Foreign.C.String (withCString)
import System.FilePath (addExtension)

data OData
type OHandle = Ptr OData

newtype OStatus =
 OSt {fromOStatus :: Int}
   deriving (Num, Show, Eq, Binary, Typeable)

type WriteAndFin = Ptr () -> Ptr () -> IO OStatus

foreign import ccall "dynamic" 
  mkWriteAndFin :: FunPtr (Addr# -> WriteAndFin) -> (Addr# -> WriteAndFin)

data OVisData = OVD {
    ovdTSxBL       :: Int
  , ovdNumChans    :: Int
  , ovdWriteAndFin :: WriteAndFin
  }

foreign import prim "vis_allocate_and_readzh"
  vis_allocate_and_read# :: Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, Int#, Int#, Addr#, Int# #)

vis_allocate_and_read :: String -> IO (Either OStatus OVisData)
vis_allocate_and_read = (`withCString` doit)
  where
    doit (Ptr addr) = IO $
      \s0 -> case vis_allocate_and_read# addr s0 of
               (# s1, hdl, ts_x_bl, chans, write_and_fin, status #) ->
                  let res = case status of
                              0# -> Right (OVD (I# ts_x_bl) (I# chans) (mkWriteAndFin (FunPtr write_and_fin) hdl))
                              _  -> Left (OSt (I# status))
                  in (# s1, res #)

writeOSKAR
    :: OVisData
    -> FilePath
    -> FilePath
    -> IO OStatus
writeOSKAR (OVD ts_x_bl chans write_and_fin) path_amp path_uvw = do
    (ptru, rawsizeu, offsetu, _sizeu) <- mmapFilePtr path_uvw ReadWriteEx (Just (0, 3 * ts_x_bl_bytes))
    (ptra, rawsizea, offseta, _sizea) <- mmapFilePtr path_amp ReadWriteEx (Just (0, 8 * ts_x_bl_bytes * chans))
    finally
      (write_and_fin (ptra `plusPtr` offseta) (ptru `plusPtr` offsetu))
      (munmapFilePtr ptru rawsizeu >> munmapFilePtr ptra rawsizea)
  where
    ts_x_bl_bytes = ts_x_bl * sizeOf (undefined :: CDouble)

uvw_filename, amp_filename :: String -> String
uvw_filename = (`addExtension` "uvw")
amp_filename = (`addExtension` "amp")

convertVis :: String -> IO OStatus
convertVis fname = do
  evd <- vis_allocate_and_read fname
  case evd of
    Left status -> return status
    Right ovd -> writeOSKAR ovd (amp_filename fname) (uvw_filename fname)
