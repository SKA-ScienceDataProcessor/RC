{-# LANGUAGE
      QuasiQuotes
    , PackageImports
  #-}

module ScatterGridderWDep where

import Text.Printf(printf)

import Data.Loc (noLoc)
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

pregridded :: [FieldGroup] -> Type
pregridded gcf_fields = [cty|
   struct Pregridded_tag
   {
     short u;
     short v;
     $sdecls:gcf_fields
   }
  |]

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

mkCfgFixSupport :: Int -> Int -> Int -> Func
mkCfgFixSupport w_planes max_supp grid_size = [cfun|
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
  
mkCfgVarSupport :: Int -> Int -> Int -> Func
mkCfgVarSupport w_planes half_supp_step =
  mkCfgFixSupport w_planes ((w_planes - 1) * half_supp_step * 2)

mkSuppOff :: Int -> Int -> Exp
mkSuppOff overu overv = [cexp| (wplane * $overu + fracu) * $overv + fracv |]

mkFixed :: Int -> Exp -> Exp
mkFixed max_supp e = [cexp| $e * $max_supp |]

mkRet0 :: Exp
mkRet0 =
  [cexp| (typename Pregridded) {
          u + uv_shift_in_pixels
        , v + uv_shift_in_pixels
        }
       |]

-- FIXME: move to Utils
addFieldsToCompoundLiteral :: Exp -> [Exp] -> Exp
addFieldsToCompoundLiteral (CompoundLit t clist loc) inits = CompoundLit t clist' loc
  where
    clist' = clist ++ map toDI inits
    toDI e = (Nothing, ExpInitializer e noLoc)
addFieldsToCompoundLiteral _ _ = error "Expression is not a compound literal!"

mkRetFix :: Int -> Int -> Int -> Exp
mkRetFix overu overv max_supp =
  addFieldsToCompoundLiteral mkRet0 [mkFixed max_supp $ mkSuppOff overu overv]

mkRetVar :: Int -> Int -> Int -> Exp
mkRetVar overu overv half_supp_step =
  addFieldsToCompoundLiteral mkRet0 [
      mkSuppOff overu overv
    , [cexp| wplane * $half_supp_step |]
    ]

mkPregrid :: Int -> Int -> Int -> Exp -> Func
mkPregrid max_supp grid_size over cle =
  let
    overu = over
    overv = over
  in [cfun|
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
        , uv_shift_in_pixels = ($max_supp + $grid_size) / 2
        ;
      short
          fracu = (short)((double)$overu * (us - (double)u))
        , fracv = (short)((double)$overv * (vs - (double)v))
        ;
     
      return $cle;
    }
  |]

mkPregridFix :: Int -> Int -> Int -> Func
mkPregridFix max_supp grid_size over =
  mkPregrid max_supp grid_size over (mkRetFix over over max_supp)

mkPregridFixCode :: Doc
mkPregridFixCode = ppr (mkPregridFix 128 2048 8)

mkPregridVar :: Int -> Int -> Int -> Int -> Func
mkPregridVar half_supp_step w_planes grid_size over =
  let max_supp = (w_planes - 1) * half_supp_step * 2
  in mkPregrid max_supp grid_size over (mkRetVar over over half_supp_step)

mkPregridVarCode :: Doc
mkPregridVarCode = ppr (mkPregridVar 5 32 2048 8)

data Inc = SI String | CI String

mkInc :: Inc -> String
mkInc (SI s) = printf "#include <%s>\n" s
mkInc (CI s) = printf "#include \"%s\"\n" s

mkIncs :: [Inc] -> String
mkIncs = concatMap mkInc

-- FIXME!
unit :: Int -> Int -> Int -> [FieldGroup] -> [Definition]
unit w_planes max_supp grid_size gcf_fields =
  [cunit|
    $esc:(mkIncs [SI "minmax.h", CI "scatter_gridder.h"])

    typedef $ty:double4c Double4c;
    typedef $ty:(pregridded gcf_fields) Pregridded;
    typedef $ty:double3 Double3;
    typedef $ty:task_cfg TaskCfg;

    $func:(mkCfgFixSupport w_planes max_supp grid_size)
  |]

mkCfgCode :: Doc
mkCfgCode = ppr (unit 32 128 2048
  [ [csdecl| short gcf_layer_index; |]
  , [csdecl| short supp_size; |]
  ])

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
