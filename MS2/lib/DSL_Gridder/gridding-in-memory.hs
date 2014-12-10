{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import Foreign.Ptr (plusPtr)
import Text.Printf (printf)

import System.IO.MMap (
    Mode(..)
  , mmapFilePtr
  , munmapFilePtr
  )
import Control.Distributed.Static (Closure)

import GriddersFFI(
    GridData
  , GStatus(..))
import Oskar (
    uvw_filename
  , amp_filename
  )

import DNA
import DGridding hiding (__remoteTable)
import qualified DGridding as DG

gridActor :: Actor (String, Closure GridderActor, GridderParams, Shell (Maybe (FilePath, GridData)) String) Int
gridActor = actor $ \(gridder_name, closure, params, wri_shell) -> do
    r <- select Local 0
    act <- startActor r closure
    sendParam params act
    waiter <- delay act
    res <- await waiter
    case res of
      Left status -> sendParam Nothing wri_shell >> return (fromGStatus status)
      Right griddata -> sendParam (Just (gridder_name ++ ".dat", griddata)) wri_shell >> return 0

remotable [ 'gridActor
          ]

doThemAll :: Actor () String
doThemAll = actor $ \_ -> do
    rsim <- select Local 0
    sim  <- startActor rsim $(mkStaticClosure 'rawSystemActor)
    sendParam ("oskar_sim_interferometer", [vis_file_name]) sim
    simwaiter <- delay sim
    retcode <- await simwaiter
    case retcode of
      Just err -> return $ "oskar_sim_interferometer failed with: " ++ show err
      Nothing -> do
        rcvt <- select Local 0
        cvt  <- startActor rcvt $(mkStaticClosure 'convertVisActor)
        sendParam vis_file_name cvt
        cvtwaiter <- delay cvt
        cvtstatus <- await cvtwaiter
        if cvtstatus /= 0
          then return $ "uvwv convertor failed with: " ++ show cvtstatus
          else do
            (ptru, rawsizeu, offsetu, sizeu) <- liftIO $ mmapFilePtr (uvw_filename vis_file_name) ReadOnly Nothing
            (ptra, rawsizea, offseta, sizea) <- liftIO $ mmapFilePtr (amp_filename vis_file_name) ReadOnly Nothing
            if sizeu /= uvw_bytes_in_chnl || sizea /=  amp_bytes_in_chnl
              then return $ "Wrong input dimensions"
              else do
                let
                   buffers = (ptru `plusPtr` offsetu, ptra `plusPtr` offseta)
                   write_closure = $(mkStaticClosure 'writeResultsActor)
                   grid_closure = $(mkStaticClosure 'gridActor)
                --
                rwri1 <- select Local 0
                wri1 <- startActor rwri1 write_closure
                waiter_wri1 <- delay wri1
                --
                rwri2 <- select Local 0
                wri2 <- startActor rwri2 write_closure
                waiter_wri2 <- delay wri2
                --
                rgridder1 <- select Local 0
                gridder1  <- startActor rgridder1 grid_closure
                waiter_gridder1 <- delay gridder1
                --
                rgridder2 <- select Local 0
                gridder2  <- startActor rgridder2 grid_closure
                waiter_gridder2 <- delay gridder2
                --
                sendParam ("Romein", $(mkStaticClosure 'romeinActor), buffers, wri1) gridder1
                sendParam ("Halide", $(mkStaticClosure 'halideActor), buffers, wri2) gridder2
                -- Ignore them for now. Writer should spit some diagnostics.
                _rcode1 <- await waiter_gridder1
                _rcode2 <- await waiter_gridder2
                --
                liftIO $ munmapFilePtr ptru rawsizeu >> munmapFilePtr ptra rawsizea
                --
                msg1 <- await waiter_wri1
                msg2 <- await waiter_wri2
                return $ printf "Finished with:\n\t%s from the 1st writer,\n\ts from the 2nd writer" msg1 msg2

main :: IO ()
main = dnaRun (DG.__remoteTable . __remoteTable) $ eval doThemAll () >>= liftIO . putStrLn
