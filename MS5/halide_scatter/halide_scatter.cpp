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

int main(/* int argc, char **argv */) {
  // Grid size
  int GRID_WIDTH = 4096;
  int GRID_HEIGHT = 4096;

  // visibilities
  int VIS = 200;

  ImageParam pos(type_of<int>(), 2);
  ImageParam over(type_of<int>(), 2);
  ImageParam vis(type_of<double>(), 2);

  ImageParam gcfr(type_of<double>(), 4);
  ImageParam gcfi(type_of<double>(), 4);

  Param<int> gcf_size("gcf_size");

  // UV-Grid - initially 0
  Func uvg("uvg"); Var x("x"), y("y");
  uvg(x, y) = Complex(cast<double>(0.0f), cast<double>(0.0f));

  // Grid visibilities. This is a reduction over:
  // 1. All visibilities
  // 2. All GCF coordinates (X,Y)
  RDom red(
      0, gcf_size
    , 0, VIS
    , 0, gcf_size
    );
  RVar
      rgcfx = red.x
    , rvis  = red.y
    , rgcfy = red.z
    ;
  Expr
      overx = clamp(over(0, rvis), 0, 7)
    , overy = clamp(over(1, rvis), 0, 7);
    ;
  // Do the complex arithmetic to update the grid
  Complex visC(
      vis(0, rvis)
    , vis(1, rvis)
    );
  Complex gcfC(
      gcfr(overx, overy, rgcfx, rgcfy)
    , gcfi(overx, overy, rgcfx, rgcfy)
    );
  uvg(
      clamp(pos(0, rvis) + rgcfx, 0, GRID_WIDTH - 1)
    , clamp(pos(1, rvis) + rgcfy, 0, GRID_HEIGHT - 1)
    ) += visC * gcfC;

  uvg.bound(x, 0, GRID_WIDTH);
  uvg.bound(y, 0, GRID_HEIGHT);
  uvg.vectorize(x);

  std::vector<Halide::Argument> compile_args = { pos, over, vis, gcf_size, gcfr, gcfi };
  uvg.compile_to_lowered_stmt("uvg.html", compile_args, HTML);
}
