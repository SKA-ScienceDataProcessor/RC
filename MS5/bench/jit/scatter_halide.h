#include "Halide.h"
using namespace Halide;

#include "cfg.h"

enum DimConfig {
    _VIS0, _VIS1, _GCF0, _GCF1, _GCF2, _GCF3, _UVG0, _UVG1,
    _DIM_CONFIGS
};

enum UpdConfig {
    _UPD_NONE, _UPD_VECT, _UPD_FUSE, _UPD_FUSE_UNROLL
};

struct SGridderConfig {
    int cpos, xpos, ypos, vpos;
    int dim;
    UpdConfig upd;
    int vector;
};

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
  SGridder(SGridderConfig cfg);

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
