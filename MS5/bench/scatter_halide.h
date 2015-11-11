#include "Halide.h"
using namespace Halide;

enum ComplxFields {
    _REAL = 0
  , _IMAG
  , _CPLX_FIELDS
  };

const int
    GCF_SIZE = 16
  , OVER = 8
  ;

enum VisFields {
	_U=0
  , _V
  , _W
  , _R
  , _I
  ,  _VIS_FIELDS
  };


struct SGridder {
  SGridder(int cpos, int xpos, int ypos, int vpos);

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

  SGridder& setDims();
  SGridder& strategy1();
  SGridder& strategy2();
};

extern "C" {

SGridder * genSGridder(int cpos, int xpos, int ypos, int vpos);
void finSGridder(SGridder * sp);

SGridder * SGsetDims(SGridder * sp);
SGridder * SGstrategy1(SGridder * sp);
SGridder * SGstrategy2(SGridder * sp);

}
