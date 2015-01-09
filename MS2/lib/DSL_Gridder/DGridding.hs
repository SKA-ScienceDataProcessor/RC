{-# LANGUAGE
    CPP
  , TemplateHaskell
  , BangPatterns
  #-}

#if !(defined (TIMESTEPS) && defined (BLOCKS) && defined (NR_STATIONS) && defined (CHANNELS))
#error "Not enough metrics for the task !!!"
#endif

module DGridding where

import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (sizeOf)
import Foreign.C.Types (CDouble)
import System.Process (rawSystem)
import System.Exit (ExitCode(..))
import Text.Printf (printf)
import Control.Exception(
    handle
  , SomeException
  )

import DistData
import DNA
import GriddersFFI
import Oskar


log_duration :: String -> String -> DNA a -> DNA a
log_duration cxt param = duration (printf "<< %s with %s >>" cxt param)

rawSystemActor :: Actor (String, [String]) (Maybe Int)
rawSystemActor = actor $ \(cmd, args) ->
    log_duration "rawSystemActor" (unwords $ cmd:args) $ liftIO (fmap ec2mb $ rawSystem cmd args)
  where
    -- We have no binary instance for ExitCode, hence this conversion
    ec2mb ExitSuccess = Nothing
    ec2mb (ExitFailure n) = Just n

-- No bother with special return type. 'String' at the moment.
writeResultsActor :: Actor (Maybe (FilePath, GridData)) String
writeResultsActor = actor doIt
  where
    doIt Nothing = return "Nothing to write."
    doIt (Just (f, gd)) = log_duration "writeResultsActor" f $ liftIO $ 
      handle handleEx $ do
         withDistData (RemoteDataSimple f $ Just grid_size) (`copyBytes` (castPtr $ gdData gd))
         gdFinalize gd
         return $ f ++ " written successfully."
    handleEx :: SomeException -> IO String
    handleEx = return . show

#define __SHOUT(a) putStrLn $ printf (printf "<<<%s>>>" a)

-- type GridderParams = (Ptr CDouble, Ptr CDouble)
type GridderParams = (String, String)
type GridderActor = Actor GridderParams (Either GStatus GridData)

-- mkGridderActor :: String -> GridProcType -> GridderActor
-- mkGridderActor gridder_name gridder_proc = actor $ \(uvwp, ampp) ->
--   log_duration "GridderActor" gridder_name $ liftIO $ gridder_proc uvwp ampp

mkGridderActor :: String -> GridProcType -> GridderActor
mkGridderActor gridder_name gridder_proc = actor $ log_duration "GridderActor" gridder_name . liftIO . gridProc -- $ \(uvwp, ampp) ->
  where
    gridProc (path_uvw, path_amp) =
      withDistData2
        (RemoteDataSimple path_amp Nothing)
          (RemoteDataSimple path_uvw Nothing)
            doit
    doit pa sizea pu sizeu = do
      if sizeu /= uvw_bytes_in_chnl || sizea /= amp_bytes_in_chnl
        then return (Left (-999)) -- FIXME: better retcode
        else gridder_proc (castPtr pu) (castPtr pa)

romeinActor, halideActor :: GridderActor
romeinActor = mkGridderActor "Romein" romeinComputeGridOnCuda
halideActor = mkGridderActor "Halide" halideComputeGridOnCuda

convertVisActor :: Actor String OStatus
convertVisActor = actor $ \fname -> log_duration "convertVisActor" fname $ liftIO (convertVis fname)

-- TODO: Add parameterts setting *and* oskar_sim_interferometer .ini file generation
grid_size :: Int
grid_size = 2048 * 2048 * 8 * sizeOf (undefined :: CDouble)

vis_file_name :: String
vis_file_name = "360-1.vis"

ts_per_block, blocks, stations, channels :: Int
ts_per_block = TIMESTEPS
blocks = BLOCKS
stations = NR_STATIONS
channels = CHANNELS

baselines :: Int
baselines = stations * (stations - 1) `div` 2

elems_in_chnl :: Int
elems_in_chnl = ts_per_block * blocks * baselines

uvw_doubles_in_chnl, amp_doubles_in_chnl :: Int
uvw_doubles_in_chnl = elems_in_chnl * 3
amp_doubles_in_chnl = elems_in_chnl * 8

uvw_bytes_in_chnl, amp_bytes_in_chnl :: Int
uvw_bytes_in_chnl = uvw_doubles_in_chnl * sizeOf (undefined :: CDouble)
amp_bytes_in_chnl = amp_doubles_in_chnl * sizeOf (undefined :: CDouble)

remotable [ 'rawSystemActor
          , 'convertVisActor
          , 'romeinActor
          , 'halideActor
          , 'writeResultsActor
          ]
