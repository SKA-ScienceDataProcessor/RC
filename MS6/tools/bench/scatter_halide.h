#include "Halide.h"
using namespace Halide;

#include "cfg.h"

enum ComplxFields {
    _REAL = 0
  , _IMAG
  , _CPLX_FIELDS
  };

enum VisFields {
	_U=0
  , _V
  , _W
  , _R
  , _I
  ,  _VIS_FIELDS
  };

struct SGridder {
  SGridder(
      int cpos
    , int xpos
    , int ypos
    , int vpos
    );

  Param<double> scale;
  Param<int>    grid_size;
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
