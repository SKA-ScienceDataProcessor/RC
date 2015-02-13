{-# LANGUAGE
      QuasiQuotes
    , PackageImports
  #-}

module ScatterGridderWDep where

import "language-c-quote" Language.C.Syntax
import Language.C.Quote.CUDA
import Text.PrettyPrint.Mainland(
    Doc
  , ppr
  )

speed_of_light, wstep_correct :: Double
speed_of_light = 299792458.0
wstep_correct = 0.00001

vis_typ, pg_typ, uvw_typ, task_cfg :: Type
vis_typ = [cty| typename complex [4] |]
pg_typ = [cty|
   typedef struct Pregridded_tag
   {
     short u;
     short v;
     short gcf_layer_index;
     short supp_size;
   } Pregridded
  |]
uvw_typ = [cty|
   typedef struct Double3_tag
   {
     double u;
     double v;
     double w;
   } Double3
  |]
task_cfg = [cty|
  typedef struct TaskCfg_tag {
    double
        min_wave_length
      , max_inverse_wave_length
      , cellsize
      , cellsizeWL
      , scale
      , scaleWL
      , w_step
      , w_stepWL
      , w_shift
      , w_shiftWL
      ;
  } TaskCfg
  |]

mkCfg :: Int -> Int -> Int -> Func
mkCfg w_planes max_supp grid_size = [cfun|
  /* $ty:task_cfg */
  typename TaskCfg
   mkCfg (
      double min_u
    , double max_u
    , double min_v
    , double max_v
    , double min_w
    , double max_w
    , double max_freq
    ) {

    const int uv_shift_in_pixels = ($max_supp + $grid_size) / 2;

    double
        min_wavelength = $speed_of_light / max_freq
      , max_inverse_wave_length = max_freq / $speed_of_light
      , maxx_u = max(max_u, -min_u)
      , maxx_v = max(max_v, -min_v)
      , maxx = max(maxx_u, maxx_v)
      , cellsize = maxx / (double)uv_shift_in_pixels
      , cellsizeWL = cellsize * max_inverse_wave_length
      , scale = (double)uv_shift_in_pixels / maxx
      , scaleWL = scale * min_wavelength
      , w_step = (max_w - min_w) / (double)$w_planes + $wstep_correct
      , w_stepWL = w_step * scaleWL
      , w_shift = -min_w
      , w_shiftWL = w_shift * scaleWL
      ;
    /* return ($ty:task_cfg) { */
    return (typename TaskCfg) {
        min_wavelength
      , max_inverse_wave_length
      , cellsize
      , cellsizeWL
      , scale
      , scaleWL
      , w_step
      , w_stepWL
      , w_shift
      , w_shiftWL
      };
    }
  |]

mkCfgCode :: Doc
mkCfgCode = ppr (mkCfg 32 128 2048)
