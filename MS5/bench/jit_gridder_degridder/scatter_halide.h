#pragma once

#include "Halide.h"
using namespace Halide;

enum VisFields {
    _R = _UVWFields
  , _I
  , _VIS_FIELDS
  };

struct SGridder {
  SGridder(int cpos, int xpos, int ypos, int vpos);

  Param<double> scale;
  ImageParam
      vis
    , gcf_fused
    ;
  RVar
      rcmplx
    , rgcfx
    , rgcfy
    , rvis
    ;
  Func
      uvg
    , uv
    , inBound
    , overc
    ;
};
