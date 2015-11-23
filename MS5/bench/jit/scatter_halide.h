#include "Halide.h"
using namespace Halide;

#include "cfg.h"

enum DimConfig {
    _VIS0, _VIS1, _GCF0, _GCF1, _GCF2, _GCF3, _UVG0, _UVG1,
    _DIM_CONFIGS
};

enum UpdConfig {
    _UPD_NONE, _UPD_VECT, _UPD_FUSE, _UPD_FUSE_UNROLL, _UPD_UNROLL
};

struct SGridderConfig {
    int gcfSize;
    int gridSize;
    int cpos, xpos, ypos, vpos, blpos; // complex, uvgx, uvgy, vis, bl
    int dim;
    UpdConfig upd;
    int vector;
    int steps; // visibilities per baseline
    int chunks; // baselines per task
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
    , rbl
    , rstep
    ;
  Func
      uvg
    , uv
    , inBound
    , overc
    ;

};

extern "C" {

SGridder * genSGridder(int cpos, int xpos, int ypos, int vpos);
void finSGridder(SGridder * sp);

SGridder * SGsetDims(SGridder * sp);
SGridder * SGstrategy1(SGridder * sp);
SGridder * SGstrategy2(SGridder * sp);

}
