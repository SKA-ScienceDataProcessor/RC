{-# LANGUAGE
    CPP
  , TemplateHaskell
  #-}

module DGridding where

import System.IO (
    openBinaryFile
  , hClose
  , IOMode(..)
  , hPutBuf
  )
import Control.Exception (
    bracket
  , finally
  )
import System.IO.MMap (
    Mode(..)
  , mmapFilePtr
  , munmapFilePtr
  )
import Foreign.Ptr (
    Ptr
  , plusPtr
  , ptrToIntPtr
  , intPtrToPtr
  )
import Foreign.Storable (sizeOf)
import Foreign.C.Types (CDouble)
import System.FilePath (addExtension)
import System.Process (rawSystem)
import System.Exit (ExitCode(..))
import Text.Printf (printf)
import Control.Exception(
    handle
  , SomeException
  )

import DNA
import GriddersFFI
import Oskar

rawSystemActor :: Actor (String, [String]) (Maybe Int)
rawSystemActor = actor $ \(cmd, args) -> liftIO (fmap ec2mb $ rawSystem cmd args)
  where
    -- We have no binary instance for ExitCode, hence this conversion
    ec2mb ExitSuccess = Nothing
    ec2mb (ExitFailure n) = Just n

writeBinFile :: FilePath -> Ptr CDouble -> Int -> IO ()
writeBinFile f p n = bracket (openBinaryFile f WriteMode) hClose
    (\h -> hPutBuf h p n)

-- No bother with special return type. 'String' at the moment.
writeResultsActor :: Actor (Maybe (FilePath, GridData)) String
writeResultsActor = actor doIt
  where
    doIt Nothing = return "Nothing to write."
    doIt (Just (f, gd)) = liftIO $ 
      handle handleEx $ do
         writeBinFile f (gdData gd) grid_size
         gdFinalize gd
         return $ f ++ " written successfully."
    handleEx :: SomeException -> IO String
    handleEx = return . show

#define __SHOUT(a) putStrLn $ printf (printf "<<<%s>>>" a)

type GridderParams = (Ptr CDouble, Ptr CDouble)
type GridderActor = Actor GridderParams (Either GStatus GridData)

mkGridderActor :: String -> GridProcType -> GridderActor
mkGridderActor gridder_name gridder_proc = actor $ \(uvwp, ampp) -> liftIO $ do
    __SHOUT("Start %s gridding") gridder_name
    rr <- gridder_proc uvwp ampp
    __SHOUT("Finished %s gridding") gridder_name
    return rr

romeinActor, halideActor :: GridderActor
romeinActor = mkGridderActor "Romein" romeinComputeGridOnCuda
halideActor = mkGridderActor "Romein" halideComputeGridOnCuda

convertVisActor :: Actor String OStatus
convertVisActor = actor $ liftIO . convertVis

-- TODO: Add parameterts setting *and* oskar_sim_interferometer .ini file generation
grid_size :: Int
grid_size = 2048 * 2048 * 8 * sizeOf (undefined :: CDouble)

vis_file_name :: String
vis_file_name = "360-1.vis"

ts_per_block, blocks, stations, channels :: Int
ts_per_block = 10
blocks = 36
stations = 30
channels = 1

baselines :: Int
baselines = stations * (stations - 1) `div` 2

elems_in_chnl :: Int
elems_in_chnl = ts_per_block * blocks * baselines

uvw_doubles_in_chnl, amp_doubles_in_chnl :: Int
uvw_doubles_in_chnl = elems_in_chnl * 3
amp_doubles_in_chnl = elems_in_chnl * 8

uvw_bytes_in_chnl, amp_bytes_in_chnl :: Int
uvw_bytes_in_chnl = uvw_doubles_in_chnl * sizeOf (undefined :: CDouble)
amp_bytes_in_chnl = uvw_doubles_in_chnl * sizeOf (undefined :: CDouble)

remotable [ 'rawSystemActor
          , 'convertVisActor
          , 'romeinActor
          , 'halideActor
          , 'writeResultsActor
          ]
