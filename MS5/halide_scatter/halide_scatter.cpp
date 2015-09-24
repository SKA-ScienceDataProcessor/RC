#include "Halide.h"
#include <stdio.h>

using namespace Halide;

struct Complex {
  Expr real, imag;
  Complex(Tuple t) : real(t[0]), imag(t[1]) {}
  Complex(Expr r, Expr i) : real(r), imag(i) {}
  Complex(FuncRefExpr t) : Complex(Tuple(t)) {}
  Complex(FuncRefVar t) : Complex(Tuple(t)) {}
  operator Tuple() const {
    return{ real, imag };
  }
  Complex operator+(const Complex &other) const {
    return{ real + other.real, imag + other.imag };
  }
  Complex operator*(const Complex &other) const {
    return{ real * other.real - imag * other.imag,
      real * other.imag + imag * other.real };
  }
};

inline void operator += (FuncRefExpr fref, const Complex &cplx) {
  fref = Complex(fref) + cplx;
}

#define P(a) a(#a)
#define IP(a,b,c) a(b,c,#a)

int main(/* int argc, char **argv */) {
  int
      wplanes = 59
    , over = 8
    ;
  
  Param<double>
      P(scale)
    , P(wstep)
    ;
  Param<int>
      P(max_supp_here)
    , P(ts_ch)
    , P(grid_pitch)
    , P(grid_size)
    ;
  ImageParam IP(gcf_supps, type_of<int>(), 1);
  // They are in fact interleaved, so we declare them separately
  ImageParam
      IP(uf, type_of<double>(), 1)
    , IP(vf, type_of<double>(), 1)
    , IP(wf, type_of<double>(), 1)
    ;
  // Also interleaved, so we declare them separately too
  ImageParam
      IP(visr, type_of<double>(), 1)
    , IP(visi, type_of<double>(), 1)
    ;
  ImageParam IP(gcfoff, type_of<int>(), 1);
  // Interleaved
  ImageParam
      IP(gcfr, type_of<double>(), 1)
    , IP(gcfi, type_of<double>(), 1)
    ;

  Func
      P(us)
    , P(vs)
    , P(u)
    , P(v)
    , P(w)
    , P(overx)
    , P(overy)
    ;
  Var P(t);
  us(t) = uf(t) * scale;
  vs(t) = vf(t) * scale;
  overx(t) = cast(Int(16), over * (us(t) - floor(us(t))));
  overy(t) = cast(Int(16), over * (vs(t) - floor(vs(t))));
  u(t) = cast(Int(16), us(t) + cast(Int(16), grid_size / 2 - max_supp_here / 2));
  v(t) = cast(Int(16), vs(t) + cast(Int(16), grid_size / 2 - max_supp_here / 2));
  w(t) = cast(Int(16), wf(t) / wstep);
  
  
  // UV-Grid - initially 0
  Func P(uvg); Var P(x), P(y);
  uvg(x, y) = Complex(cast<double>(0.0f), cast<double>(0.0f));

  // Grid visibilities. This is a reduction over:
  // 1. All visibilities
  // 2. All GCF coordinates (X,Y)
  RDom red(
      0, max_supp_here
    , 0, ts_ch
    , 0, max_supp_here
    );
  RVar
      rgcfx = red.x
    , rvis  = red.y
    , rgcfy = red.z
    ;

  #define TMPLIM 100000000 // Should be full GCF data size here. Put some provisional value to make halide happy
  // No layer correction yet
  Expr off = clamp (gcfoff(w(rvis)) + overx(rvis) * gcf_supps(w(rvis)) + overy(rvis), 0, TMPLIM);
  // Do the complex arithmetic to update the grid
  Complex visC(
      visr(rvis)
    , visi(rvis)
    );
  Complex gcfC(
      gcfr(off)
    , gcfi(off)
    );
  uvg(
      clamp(u(rvis) + rgcfx, 0, grid_pitch - 1)
    , clamp(v(rvis) + rgcfy, 0, grid_size - 1)
    ) += visC * gcfC;

  uvg.bound(x, 0, grid_pitch);
  uvg.bound(y, 0, grid_size);

  // Temp. disable. It wants constants only.
  // uvg.vectorize(x);

  uvg.compute_root();

  Func uvgsh("uvgsh");
  Var P(gu), P(gv);
  uvgsh(gu, gv) = uvg(gu, gv);

  std::vector<Halide::Argument> compile_args = {
       scale
     , wstep
     , max_supp_here
     , ts_ch
     , grid_pitch
     , grid_size
     , gcf_supps
     , uf, vf, wf
     , visr, visi
     , gcfoff
     , gcfr, gcfi
     };
  // uvg.compile_to_lowered_stmt("uvg.html", compile_args, HTML);
  uvgsh.compile_to_lowered_stmt("uvg.html", compile_args, HTML);
}
