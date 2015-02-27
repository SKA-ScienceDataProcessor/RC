{-# LANGUAGE
      QuasiQuotes
    , PackageImports
  #-}

module ScatterGridderWDep where

import "language-c-quote" Language.C.Syntax
import Language.C.Quote.CUDA
import Language.C.Smart ()
import Text.PrettyPrint.Mainland (
    Doc
  , ppr
  , pretty
  )
import Development.Shake
import Development.Shake.FilePath

import Utils

speed_of_light, wstep_correct :: Double
speed_of_light = 299792458.0
wstep_correct = 0.00001

data GCFType =
    GCFFixed { maxSupp :: Int }
  | GCFVar { halfSuppStep :: Int }

pregridded :: GCFType -> Type
pregridded gcftype = [cty|
   struct Pregridded_tag
   {
     short u;
     short v;
     $sdecls:(gcf_fields gcftype)
   }
  |]
  where
    gcf_fields (GCFFixed _) =
      [ [csdecl| int gcf_layer_offset; |]
      ]
    gcf_fields (GCFVar _) =
      [ [csdecl| short gcf_layer_index; |]
      , [csdecl| short half_supp_size; |]
      ]

double4c, double3, task_cfg :: Type
double4c = [cty|
   struct Double4c_tag
   {
     typename complexd XX;
     typename complexd XY;
     typename complexd YX;
     typename complexd YY;
   }
  |]
double3 = [cty|
   struct Double3_tag
   {
     double u;
     double v;
     double w;
   }
  |]
task_cfg = [cty|
  struct TaskCfg_tag {
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
  }
  |]

max_supp :: GCFType -> Int -> Int
max_supp (GCFFixed n) _ = n
max_supp (GCFVar half_supp_step) w_planes = (w_planes - 1) * half_supp_step * 2

mkCfgSupport :: GCFType -> Int -> Int -> Func
mkCfgSupport gcftype grid_size w_planes = [cfun|
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
    
    const int uv_shift_in_pixels = ($(max_supp gcftype w_planes) + $grid_size) / 2;
    
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
  
mkPregrid :: GCFType -> Int -> Int -> Int -> Func
mkPregrid gcftype grid_size w_planes over =
  [cfun|
    inline
    typename Pregridded
    pregrid(const typename Double3 * uvw, const typename TaskCfg * cfg) {
      double
          us = uvw->u * cfg->scaleWL
        , vs = uvw->v * cfg->scaleWL
        ;
      int
          u = (int)us
        , v = (int)vs
        , wplane = (int)((uvw->w + cfg->w_shift)/ cfg->w_step)
        , uv_shift_in_pixels = ($(max_supp gcftype w_planes) + $grid_size) / 2
        ;
      short
          fracu = (short)((double)$overu * (us - (double)u))
        , fracv = (short)((double)$overv * (vs - (double)v))
        ;
      return $(ret0 `addFields` extraFields);
    }
  |]
  where
    ret0 =
      [cexp|
        (typename Pregridded) {
            u + uv_shift_in_pixels
          , v + uv_shift_in_pixels
          }
      |]
    suppOff =
      [cexp| (wplane * $overu + fracu) * $overv + fracv |]
    extraFields =
      case gcftype of
        GCFFixed max_s ->
            [ suppOff * ex max_s ]
        GCFVar half_supp_step ->
            [ suppOff
            , [cexp| wplane * $half_supp_step |]
            ]
    overu = over
    overv = over


mkPregridFixCodeTest :: Doc
mkPregridFixCodeTest = ppr (mkPregrid (GCFFixed 128) 2048 32 8)

mkPregridVarCodeTest :: Doc
mkPregridVarCodeTest = ppr (mkPregrid (GCFVar 5) 2048 32 8)

unit :: GCFType -> Int -> Int -> Int -> [Definition]
unit gcf_type grid_size w_planes over =
  [cunit|
    $esc:(mkIncs [SI "minmax.h", CI "scatter_gridder.h"])

    typedef $ty:double4c Double4c;
    typedef $ty:(pregridded gcf_type) Pregridded;
    typedef $ty:double3 Double3;
    typedef $ty:task_cfg TaskCfg;

    $func:(mkCfgSupport gcf_type grid_size w_planes)
    $func:(mkPregrid gcf_type grid_size w_planes over)
  |]
  where
    

mkCfgCodeFixed, mkCfgCodeVar :: Doc
mkCfgCodeFixed = ppr (unit (GCFFixed 128) 2048 32 8)
mkCfgCodeVar = ppr (unit (GCFVar 5) 2048 32 8)

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["scatter_gridder_fixed.o", "scatter_gridder_var.o"]
  mkdep "scatter_gridder_fixed.c" mkCfgCodeFixed
  mkdep "scatter_gridder_var.c" mkCfgCodeVar
  "*.o" %> \out -> do
      let src = out -<.> "c"
      need [src]
      cmd "gcc -std=gnu11 -c -o" [out] src
  where
    mkdep outname src =
      outname %> \out -> do
         need ["ScatterGridderWDep.hs"] -- look at myself
         liftIO $ writeFile out (pretty 80 src)
