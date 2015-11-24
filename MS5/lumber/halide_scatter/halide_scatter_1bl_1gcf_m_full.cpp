#include "Halide.h"

#include <utility>
#include <vector>

using namespace Halide;

const Expr
    _REAL = 0
  , _IMAG = 1
  ;
		
const int _CMPLX_SIZE = 2;

struct Complex {
  Expr real, imag;
  Complex(Expr r, Expr i) : real(r), imag(i) {}
  Complex operator+(const Complex &other) const {
    return{ real + other.real, imag + other.imag };
  }
  Complex operator*(const Complex &other) const {
    return{
        real * other.real - imag * other.imag
      , real * other.imag + imag * other.real
      };
  }
  Expr unpack(Expr c) { return select(c == _REAL, real, imag); }
};

/* Not need it ATM
inline Func flushAndContinue(const char * new_name, Func & f) {
  Func ret(new_name);
  f.compute_root();
  ret(f.args()) = f(f.args());
  return ret;
}
*/

#define P(a) a(#a)
#define IP(a,b,c) a(b,c,#a)

// We generate the kernel for GPU which works on the subset of baselines
//   having the same convolution size as we did in MS3.
// But we also simplify the algorithm a bit using
//  GCF-per-baseline (with the size equal to convolution size)
//  instead of GCF-per-point approach.
//  We've done this for all halide_scatter_1bl_1gcf... family of gridders.
int main(/* int argc, char **argv */) {
  int
      over = 8
    , grid_size = 2048
    , max_gcf_size = 128
    ;

  Param<double>
      P(scale)
    , P(wstep)
    ;
  Param<int>
      P(gcf_layer_size)
    , P(num_of_baselines)
    , P(ts_ch)
    ;
  ImageParam IP(uvwf, type_of<double>(), 3);
  ImageParam IP(vis, type_of<double>(), 3);
  ImageParam IP(gcf, type_of<double>(), 2);
  ImageParam IP(vismap, type_of<int>(), 1);

  Func
      P(uvs)
    , P(uv)
    , P(overc)
    , P(gcfoff)
    , P(bloff)
    ;

  Var P(t)
    , P(uvdim)
    , P(bl)
    ;
  const Expr
      _U = 0
    , _V = 1
    ;
  
  uvs(uvdim, t, bl) = uvwf(uvdim, t, bl) * scale;
  overc(uvdim, t, bl) = clamp(cast<int>(round(over * (uvs(uvdim, t, bl) - floor(uvs(uvdim, t, bl))))), 0, 7);
  uv(uvdim, t, bl) = cast<int>(round(uvs(uvdim, t, bl)) + grid_size / 2 - gcf_layer_size / 2);
  gcfoff(t, bl) = (overc(_U, t, bl) * over + overc(_V, t, bl)) * gcf_layer_size * gcf_layer_size;
  bloff(bl) = clamp(ts_ch * vismap(bl), 0, ts_ch * num_of_baselines);

  RDom red(
      0, _CMPLX_SIZE
    , 0, gcf_layer_size
    , 0, ts_ch
    , 0, gcf_layer_size
    , 0, num_of_baselines
    );
  RVar P(rcmplx), P(rgcfx), P(rvis), P(rgcfy), P(rbl0);
  rcmplx = red.x;
  rgcfx  = red.y;
  rvis   = red.z;
  rgcfy  = red.w;
  rbl0   = red[4];
  Expr rbl = bloff(rbl0);

  Expr gcf_pix_off = gcfoff(rvis, rbl) + rgcfx + rgcfy * gcf_layer_size; 
  Complex gcfC(
      gcf(_REAL, gcf_pix_off)
    , gcf(_IMAG, gcf_pix_off)
    );

  Complex visC(
      vis(_REAL, rvis, rbl)
    , vis(_IMAG, rvis, rbl)
    );

  Func P(uvg);
  Var P(cmplx)
    , P(x)
    , P(y)
    ;
  uvg(cmplx, x, y) = undef<double>();

  Expr
      clampedU = clamp(uv(_U, rvis, rbl), 0, grid_size - 1 - max_gcf_size)
    , clampedV = clamp(uv(_V, rvis, rbl), 0, grid_size - 1 - max_gcf_size)
    , U = clampedU + rgcfx
    , V = clampedV + rgcfy
    ;

  uvg(
      rcmplx
    , U
    , V
    ) += (visC * gcfC).unpack(rcmplx)
    ;

  uvg.bound(cmplx, 0, _CMPLX_SIZE);
  uvg.bound(x, 0, grid_size);
  uvg.bound(y, 0, grid_size);

  std::vector<Halide::Argument> compile_args = {
      scale
    , wstep
    , num_of_baselines
    , ts_ch
    , vismap
    , uvwf
    , vis
    , gcf_layer_size
    , gcf
  };
  uvg.compile_to_lowered_stmt("uvg11.html", compile_args, HTML);
  uvg.compile_to_file(
      "uvg11_full"
    , compile_args
    , get_target_from_environment()
        .with_feature(Target::CUDA)
        .with_feature(Target::CUDACapability35)
    );
}
