#include "Halide.h"

#include <utility>
#include <vector>

using namespace Halide;

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
  int over = 8;

  Param<double>
      P(scale)
    , P(wstep)
    ;
  Param<int>
      P(num_of_baselines)
      // , P(max_supp_here)
    , P(ts_ch)
    , P(grid_size)
    ;
  ImageParam IP(supports, type_of<int>(), 1);
  ImageParam IP(gcf_supps, type_of<int>(), 1);
  ImageParam IP(uvwf, type_of<double>(), 3);
  ImageParam IP(vis, type_of<double>(), 3);
  ImageParam IP(gcfoff, type_of<int>(), 1);

  // We don't make additional complex dimension ATM
  // because we will need all 4 available dimensions
  // to address GCF if we stick to one-GCF-layer-per-baseline
  // approach.
  // Interleaved.
  ImageParam
      IP(gcfr, type_of<double>(), 1)
    , IP(gcfi, type_of<double>(), 1)
    ;
  Func
      P(uvs)
    , P(uv)
    , P(w)
    , P(overc)
    ;

  Var P(t)
    , P(uvdim)
    , P(bl)
    ;
  const Expr
      _U = 0
    , _V = 1
    , _W = 2
    ;
  
  uvs(uvdim, t, bl) = uvwf(uvdim, t, bl) * scale;
  overc(uvdim, t, bl) = cast(Int(16), over * (uvs(uvdim, t, bl) - floor(uvs(uvdim, t, bl))));
  uv(uvdim, t, bl) = cast(Int(16), uvs(uvdim, t, bl) + cast(Int(16), grid_size / 2 - supports(bl) / 2));
  w(t, bl) = cast(Int(16), uvwf(_W, t, bl) / wstep);

  // If we compute_root them.
  // uvs.bound(uvdim, 0, 2);
  // overc.bound(uvdim, 0, 2);
  // uv.bound(uvdim, 0, 2);

  // UV-Grid - initially 0
  Func P(uvg);
  Var P(cmplx)
    , P(x)
    , P(y)
    ;
  uvg(cmplx, x, y) = cast<double>(0.0f);
  
  // Grid visibilities. This is a reduction over:
  // 0. All baselines
  // 1. All visibilities
  // 2. All GCF coordinates (X,Y)
  // Provisional limit only to experiment with things
#define TMP_LIM 256
  RDom red(
      0, num_of_baselines
    , 0, TMP_LIM
    , 0, ts_ch
    , 0, TMP_LIM
    );

  RVar
      rbl   = red.x
    , rgcfx = red.y
    , rvis  = red.z
    , rgcfy = red.w
    ;

  const Expr
      _REAL = 0
    , _IMAG = 1
    ;

  Param<int> P(gcf_data_size);
  // No layer correction yet
  Expr off = clamp(gcfoff(w(rvis, rbl)) + overc(_U, rvis, rbl) * gcf_supps(w(rvis, rbl)) + overc(_V, rvis, rbl), 0, gcf_data_size - 1);
  // Do the complex arithmetic to update the grid
  Complex visC(
      vis(_REAL, rvis, rbl)
    , vis(_IMAG, rvis, rbl)
    );
  Complex gcfC(
      gcfr(off)
    , gcfi(off)
    );

  auto clamped_uv = [=](Expr c) {
    return clamp(uv(c, rvis, rbl) + rgcfx, 0, grid_size - 1);
  };
    
  FuncRefExpr uvRef_real = uvg(
      _REAL
    , clamped_uv(_U)
    , clamped_uv(_V)
    );

  FuncRefExpr uvRef_imag = uvg(
      _IMAG
    , clamped_uv(_U)
    , clamped_uv(_V)
    );

  Expr inSupp = rgcfx < supports(rbl) && rgcfy < supports(rbl);
  uvRef_real = select(inSupp, uvRef_real + (visC * gcfC).real, uvRef_real);
  uvRef_imag = select(inSupp, uvRef_imag + (visC * gcfC).imag, uvRef_imag);

  uvg.bound(cmplx, 0, 2);
  uvg.bound(x, 0, grid_size);
  uvg.bound(y, 0, grid_size);

  // Temp. disable. It wants constants only.
  // uvg.vectorize(x);

/*
  Func uvgsh_full = flushAndContinue("uvgsh_full", uvg);
  uvgsh_full.output_buffers()[0].set_stride(0, Expr());
  uvgsh_full.output_buffers()[1].set_stride(0, Expr());
  */

  std::vector<Halide::Argument> compile_args = {
      scale
    , wstep
    , num_of_baselines
    , supports
    , ts_ch
    , grid_size
    , gcf_supps
    , uvwf
    , vis
    , gcf_data_size
    , gcfoff
    , gcfr, gcfi
  };
  uvg.compile_to_lowered_stmt("uvg.html", compile_args, HTML);
  // uvgsh_full.compile_to_lowered_stmt("uvg_full.html", compile_args, HTML);

  Target target(
      // Target::Windows
      Target::Linux
    , Target::X86, 64
    , { Target::SSE41
      , Target::AVX
      }
  );
  uvg.compile_to_file("uvg_full", compile_args, target);
  // uvgsh_full.compile_to_file("uvgsh_full", compile_args, target);
}
