#include <Halide.h>
using namespace Halide;

enum UVWFields {
	_U=0
  , _V
  , _W
  ,  _UVWFields
  };

struct SDeGridder {
  SDeGridder(
      int cpos
    , int xpos
    , int ypos
    );

  Param<double> scale;
  Param<int>    grid_size;
  ImageParam
      uvw
    , uvg
    , gcf_fused
    ;
  RVar
      rcmplx
    , rgcfx
    , rgcfy
    ;
  Func
      vis
    , uv
    , inBound
    , overc
    ;
};
