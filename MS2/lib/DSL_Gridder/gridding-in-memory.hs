{-# LANGUAGE
      CPP
    , TemplateHaskell
    , OverloadedStrings
  #-}

#if !defined FREQ_START
#error "Starting frequency is undefined !!!"
#endif

module Main(main) where

-- import Text.Printf (printf)
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
   (os, sky_model, vis_name, ini_name) = mk_ska1low_test_cfg "ska1sur.sky" FREQ_START 1 72 1800 "../../../../uvwsim/tele_sur"
  in do
       writeFile "ska1sur.sky" sky_model
       writeFile ini_name (showSettings os)
       return (vis_name, ini_name)

gridActor :: Actor (String, Closure GridderActor, GridderParams) (Int,String)
gridActor = actor $ \(gridder_name, closure, params) -> do
    res <- evalClosure closure params
    case res of
      Left  status   -> do
          r <- eval writeResultsActor Nothing
          return (fromGStatus status, r)
      Right griddata -> do
          r  <- eval writeResultsActor (Just (gridder_name ++ ".dat", griddata))
          return (0, r)

remotable [ 'gridActor
          ]

doThemAll :: Actor () String
doThemAll = actor $ \_ -> do
    (vis_file_name, ini_name) <- liftIO mkCfg
    let rawsys = $(mkStaticClosure 'rawSystemActor)
    --
    rgcf <- select Remote (N 1)
    gcf <- startActor rgcf rawsys
    sendParam ("GCF", []) gcf
    gcfwaiter <- delay Remote gcf
    --
    retcode <- eval rawSystemActor ("oskar_sim_interferometer", [ini_name])
    case retcode of
      Just err -> return $ "oskar_sim_interferometer failed with: " ++ show err
      _ -> do
        cvtstatus <- eval convertVisActor vis_file_name
        if cvtstatus /= 0
          then return $ "uvwv convertor failed with: " ++ show cvtstatus
          else do
            let
               fnames = (uvw_filename vis_file_name, amp_filename vis_file_name)
               grid_closure = $(mkStaticClosure 'gridActor)
            --
            rgridder1 <- select Remote (N 1)
            gridder1  <- startActor rgridder1 grid_closure
            waiter_gridder1 <- delay Remote gridder1
            --
            rgridder2 <- select Remote (N 1)
            gridder2  <- startActor rgridder2 grid_closure
            waiter_gridder2 <- delay Remote gridder2
            --
            sendParam ("Romein_f", $(mkStaticClosure 'romeinActor_f), fnames) gridder1
            sendParam ("Romein", $(mkStaticClosure 'romeinActor), fnames) gridder2
            -- Ignore them for now. Writer should spit some diagnostics.
            (_rcode1,_msg1) <- await waiter_gridder1
            (_rcode2,_msg2) <- await waiter_gridder2
            --
            gcfret <- await gcfwaiter
            case gcfret of
              Just err -> return $ "GCF creation failed with: " ++ show err
              _ -> do
                     binres <- eval rawSystemActor ("binner", [vis_file_name])
                     case binres of
                       Just err -> return $ "binner failed with: " ++ show err
                       _ -> do
                         bgridderres <- eval rawSystemActor ("RunGridder1", [])
                         case bgridderres of
                           Just err -> return $ "Simple CUDA gridder failed with: " ++ show err
                           _ -> return "All is good and well!!!"


main :: IO ()
main = dnaRun (DG.__remoteTable . __remoteTable) $ eval doThemAll () >>= liftIO . putStrLn
