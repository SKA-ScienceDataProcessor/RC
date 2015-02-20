{-# LANGUAGE
      QuasiQuotes
    , PackageImports
  #-}

module ScatterGridderWDep where

import Text.Printf(printf)

import "language-c-quote" Language.C.Syntax
import Language.C.Quote.CUDA
import Text.PrettyPrint.Mainland(
    Doc
  , ppr
  , pretty
  )
import Development.Shake
import Development.Shake.FilePath

speed_of_light, wstep_correct :: Double
speed_of_light = 299792458.0
wstep_correct = 0.00001

double4c, pregridded, double3, task_cfg :: Type
double4c = [cty|
   struct Double4c_tag
   {
     typename complexd XX;
     typename complexd XY;
     typename complexd YX;
     typename complexd YY;
   }
  |]
pregridded = [cty|
   struct Pregridded_tag
   {
     short u;
     short v;
     short gcf_layer_index;
     short supp_size;
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

mkCfg :: Int -> Int -> Int -> Func
mkCfg w_planes max_supp grid_size = [cfun|
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

data Inc = SI String | CI String

mkInc :: Inc -> String
mkInc (SI s) = printf "#include <%s>\n" s
mkInc (CI s) = printf "#include \"%s\"\n" s

mkIncs :: [Inc] -> String
mkIncs = concatMap mkInc

unit :: Int -> Int -> Int -> [Definition]
unit w_planes max_supp grid_size = [cunit|
  $esc:(mkIncs [SI "minmax.h", CI "scatter_gridder.h"])

  typedef $ty:double4c Double4c;
  typedef $ty:pregridded Pregridded;
  typedef $ty:double3 Double3;
  typedef $ty:task_cfg TaskCfg;

  $func:(mkCfg w_planes max_supp grid_size)
  |]

mkCfgCode :: Doc
mkCfgCode = ppr (unit 32 128 2048)

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["scatter_gridder.o"]
  "scatter_gridder.c" %> \out -> do
     need ["ScatterGridderWDep.hs"] -- look at myself
     liftIO $ writeFile out (pretty 80 mkCfgCode)
  "*.o" %> \out -> do
      let src = out -<.> "c"
      need [src]
      cmd "gcc -std=gnu11 -c -o" [out] src
