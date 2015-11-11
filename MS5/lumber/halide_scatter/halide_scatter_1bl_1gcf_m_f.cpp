#include "Halide.h"

#include <utility>
#include <vector>

using namespace Halide;

enum ComplxFields {
  _REAL = 0,
  _IMAG,

  _CPLX_COUNT
};

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

int main(/* int argc, char **argv */) {
  int
      over = 8
    , grid_size = 2048
    , max_gcf_size = 16 // No difference with gcf_layer_size
    , gcf_layer_size = 16
    // FIXME: factor all relevant constants out
    // and share them between this code and cppcycle.cpp
    ;

  Param<double>
      P(scale)
    , P(wstep)
    ;
  Param<int>
      P(ts_ch)
    ;
  ImageParam IP(uvwf, type_of<double>(), 2);
  ImageParam IP(vis, type_of<double>(), 2);
  // gcf_fused has 2 oversample dimensions fused into one
  //  to fit into 4D parameter limit
  ImageParam IP(gcf_fused, type_of<double>(), 4);

  Func
      P(uvs)
    , P(uv)
    , P(overc)
    ;

  Var P(t)
    , P(uvdim)
    , P(bl)
    ;

  enum UvwFields {
      _U = 0
    , _V
	, _W

	, _UVW_COUNT
  };

  uvs(uvdim, t) = uvwf(uvdim, t) * scale;
  overc(uvdim, t) = clamp(cast<int>(round(over * (uvs(uvdim, t) - floor(uvs(uvdim, t))))), 0, 7);
  uv(uvdim, t) = cast<int>(round(uvs(uvdim, t)) + grid_size / 2 - gcf_layer_size / 2);

  Var P(cmplx);

// No difference in fact
#define __UNFUSE 0

#if __UNFUSE
  Var
      P(suppx)
    , P(suppy)
    , P(overx)
    , P(overy)
    ;
  Func P(gcf);
  gcf(cmplx, suppx, suppy, overx, overy) = gcf_fused(cmplx, suppx, suppy, overx + 8 * overy);
#endif

#define __MATERIALIZE 1
#define __CONST_PAD 1

#if __MATERIALIZE
  // Not interested in W here
  overc.bound(uvdim, 0, _UVW_COUNT-1);
  uv.bound(uvdim, 0, _UVW_COUNT-1);

  overc.compute_root();
  uv.compute_root();
#endif

  RDom red(
      0, _CPLX_COUNT
    , 0, gcf_layer_size
    , 0, ts_ch
    , 0, gcf_layer_size
    );
  RVar
      rcmplx = red.x
    , rgcfx = red.y
    , rvis  = red.z
    , rgcfy = red.w
    ;

  Complex gcfC(
#if __UNFUSE
      gcf(_REAL, rgcfx, rgcfy, overc(_U, rvis), overc(_V, rvis))
    , gcf(_IMAG, rgcfx, rgcfy, overc(_U, rvis), overc(_V, rvis))
#else
      gcf_fused(_REAL, rgcfx, rgcfy, overc(_U, rvis) + 8 * overc(_V, rvis))
    , gcf_fused(_IMAG, rgcfx, rgcfy, overc(_U, rvis) + 8 * overc(_V, rvis))
#endif
    );

  Complex visC(
      vis(_REAL, rvis)
    , vis(_IMAG, rvis)
    );

  Func P(uvg);
  Var P(x)
    , P(y)
    ;
  uvg(cmplx, x, y) = undef<double>();

  Expr
#if __CONST_PAD
      clampedU = clamp(uv(_U, rvis), 0, grid_size - 1 - max_gcf_size)
    , clampedV = clamp(uv(_V, rvis), 0, grid_size - 1 - max_gcf_size)
    , U = clampedU + rgcfx
    , V = clampedV + rgcfy
#else
      U = clamp(uv(_U, rvis) + rgcfx, 0, grid_size - 1)
    , V = clamp(uv(_V, rvis) + rgcfy, 0, grid_size - 1)
#endif
    ;

  uvg(
      rcmplx
    , U
    , V
    ) += (visC * gcfC).unpack(rcmplx)
    ;

  Var P(xc);
  uvg.fuse(x, cmplx, xc).vectorize(xc, 4);

  RVar rgcfxc;
  uvg.update()
	.allow_race_conditions()
	.fuse(rgcfx, rcmplx, rgcfxc)
	.vectorize(rgcfxc, 8).unroll(rgcfxc, 4);

  gcf_fused
	.set_min(0,0).set_stride(0,1).set_extent(0,_CPLX_COUNT)
	.set_min(1,0).set_stride(1,_CPLX_COUNT).set_extent(1,max_gcf_size)
	.set_min(2,0).set_stride(2,_CPLX_COUNT*max_gcf_size).set_extent(2,max_gcf_size)
	.set_min(3,0).set_stride(3,_CPLX_COUNT*max_gcf_size*max_gcf_size);

  uvwf
	.set_min(0,0).set_stride(0,1).set_extent(0,_UVW_COUNT)
	.set_stride(1,_UVW_COUNT);

  vis
	.set_min(0,0).set_stride(0,1).set_extent(0,_CPLX_COUNT)
	.set_stride(1,_CPLX_COUNT);

  uvg.output_buffer()
	.set_min(0,0).set_stride(0,1).set_extent(0,_CPLX_COUNT)
	.set_min(1,0).set_stride(1,_CPLX_COUNT);

  // Var x_outer, y_outer, x_inner, y_inner;
  // uvg.tile(x, y, x_outer, y_outer, x_inner, y_inner, 4, 4);

  std::vector<Halide::Argument> compile_args = {
      scale
    , wstep
    , ts_ch
    , uvwf
    , vis
    , gcf_fused
  };
  uvg.compile_to_lowered_stmt("uvg11.html", compile_args, HTML);
  Target target(get_target_from_environment());
  uvg.compile_to_bitcode("uvg11.bc", compile_args, target);
  uvg.compile_to_file("uvg11_full", compile_args, target);
}
