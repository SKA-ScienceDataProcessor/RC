{-# LANGUAGE
      CPP
    , TemplateHaskell
    , OverloadedStrings
  #-}

#if !defined FREQ_START
#error "Starting frequency is undefined !!!"
#endif

module Main(main) where

import Text.Printf (printf)
import Control.Distributed.Static (Closure)

import GriddersFFI(
    GridData
  , GStatus(..))
import Oskar (
    uvw_filename
  , amp_filename
  , mk_ska1low_test_cfg
  , showSettings
  )

import DNA
import DGridding hiding (__remoteTable)
import qualified DGridding as DG

mkCfg :: IO (FilePath, FilePath)
mkCfg =
  let
   (os, sky_model, vis_name, ini_name) = mk_ska1low_test_cfg "ska1low.sky" FREQ_START 1 72 1800 "../../../../uvwsim/telescope"
  in do
       writeFile "ska1low.sky" sky_model
       writeFile ini_name (showSettings os)
       return (vis_name, ini_name)

gridActor :: Actor (String, Closure GridderActor, GridderParams, Shell (Val (Maybe (FilePath, GridData))) (Val String)) Int
gridActor = actor $ \(gridder_name, closure, params, wri_shell) -> do
    r <- select Local (N 0)
    act <- startActor r closure
    sendParam params act
    waiter <- delay Local act
    res <- await waiter
    case res of
      Left status -> sendParam Nothing wri_shell >> return (fromGStatus status)
      Right griddata -> sendParam (Just (gridder_name ++ ".dat", griddata)) wri_shell >> return 0

remotable [ 'gridActor
          ]

doThemAll :: Actor () String
doThemAll = actor $ \_ -> do
    (vis_file_name, ini_name) <- liftIO mkCfg
    rsim <- select Local (N 0)
    sim  <- startActor rsim $(mkStaticClosure 'rawSystemActor)
    sendParam ("oskar_sim_interferometer", [ini_name]) sim
    simwaiter <- delay Local sim
    retcode <- await simwaiter
    case retcode of
      Just err -> return $ "oskar_sim_interferometer failed with: " ++ show err
      Nothing -> do
        rcvt <- select Local (N 0)
        cvt  <- startActor rcvt $(mkStaticClosure 'convertVisActor)
        sendParam vis_file_name cvt
        cvtwaiter <- delay Local cvt
        cvtstatus <- await cvtwaiter
        if cvtstatus /= 0
          then return $ "uvwv convertor failed with: " ++ show cvtstatus
          else do
                let
                   fnames = (uvw_filename vis_file_name, amp_filename vis_file_name)
                   write_closure = $(mkStaticClosure 'writeResultsActor)
                   grid_closure = $(mkStaticClosure 'gridActor)
                --
                rwri1 <- select Local (N 0)
                wri1 <- startActor rwri1 write_closure
                waiter_wri1 <- delay Local wri1
                --
                rwri2 <- select Local (N 0)
                wri2 <- startActor rwri2 write_closure
                waiter_wri2 <- delay Local wri2
                --
                rgridder1 <- select Local (N 0)
                gridder1  <- startActor rgridder1 grid_closure
                waiter_gridder1 <- delay Local gridder1
                --
                rgridder2 <- select Local (N 0)
                gridder2  <- startActor rgridder2 grid_closure
                waiter_gridder2 <- delay Local gridder2
                --
                sendParam ("Romein", $(mkStaticClosure 'romeinActor), fnames, wri1) gridder1
                sendParam ("Halide", $(mkStaticClosure 'halideActor), fnames, wri2) gridder2
                -- Ignore them for now. Writer should spit some diagnostics.
                _rcode1 <- await waiter_gridder1
                _rcode2 <- await waiter_gridder2
                --
                msg1 <- await waiter_wri1
                msg2 <- await waiter_wri2
                return $ printf "Finished with:\n\t%s from the 1st writer,\n\ts from the 2nd writer" msg1 msg2

main :: IO ()
main = dnaRun (DG.__remoteTable . __remoteTable) $ eval doThemAll () >>= liftIO . putStrLn
