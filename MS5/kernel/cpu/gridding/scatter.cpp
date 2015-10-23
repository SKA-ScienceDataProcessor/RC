
#include "Halide.h"

using namespace Halide;

enum ComplxFields { _REAL = 0, _IMAG,  _CPLX_FIELDS };
struct Complex {
  Expr real, imag;
  Complex(Expr r, Expr i) : real(r), imag(i) {}
  Complex(FuncRefExpr f) : real(Tuple(f)[0]), imag(Tuple(f)[1]) {}
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
  operator Tuple() { return Tuple(real, imag); }
};

int main(int argc, char **argv) {
  if (argc < 2) return 1;

  // GCF size and oversampling are constants for now
  const int GCF_SIZE = 16
          , OVER = 8;

  // ** Input

  Param<double> scale("scale");

  // Visibilities: Array of 5-pairs, packed together with UVW
  enum VisFields { _U=0, _V, _W, _R, _I,  _VIS_FIELDS };
  ImageParam vis(type_of<double>(), 2, "vis");
  vis.set_min(0,0).set_stride(0,1).set_extent(0,_VIS_FIELDS)
     .set_stride(1,_VIS_FIELDS);

  // GCF: Array of OxOxSxS complex numbers. We "fuse" two dimensions
  // as Halide only supports up to 4 dimensions.
  ImageParam gcf_fused(type_of<double>(), 4, "gcf");
  gcf_fused
     .set_min(0,0).set_stride(0,1).set_extent(0,_CPLX_FIELDS)
     .set_min(1,0).set_stride(1,_CPLX_FIELDS).set_extent(1,GCF_SIZE)
     .set_min(2,0).set_stride(2,_CPLX_FIELDS*GCF_SIZE).set_extent(2,GCF_SIZE)
     .set_min(3,0).set_stride(3,_CPLX_FIELDS*GCF_SIZE*GCF_SIZE).set_extent(3,OVER*OVER);

  std::vector<Halide::Argument> args = { scale, vis, gcf_fused };

  // ** Output

  // Grid starts out undefined so we can update the output buffer
  Func uvg("uvg");
  Var cmplx("cmplx"), x("x"), y("y");
  uvg(cmplx, x, y) = undef<double>();

  uvg.output_buffer()
    .set_min(0,0).set_stride(0,1).set_extent(0,_CPLX_FIELDS)
    .set_min(1,0).set_stride(1,_CPLX_FIELDS);
  Expr grid_size = uvg.output_buffer().height();

  // ** Helpers

  // Coordinate preprocessing
  Func uvs("uvs"), uv("uv"), overc("overc");
  Var uvdim("uvdim"), t("t");
  uvs(uvdim, t) = vis(uvdim, t) * scale;
  overc(uvdim, t) = clamp(cast<int>(round(OVER * (uvs(uvdim, t) - floor(uvs(uvdim, t))))), 0, OVER-1);
  uv(uvdim, t) = cast<int>(round(uvs(uvdim, t)) + grid_size / 2 - GCF_SIZE / 2);

  // Visibilities to ignore due to being out of bounds
  Func inBound("inBound");
  inBound(t) = abs(uvs(_U,t)) < grid_size/2 && abs(uvs(_V,t)) < grid_size/2;

  // GCF lookup for a given visibility
  Func gcf("gcf");
  Var suppx("suppx"), suppy("suppy"), overx("overx"), overy("overy");
  gcf(suppx, suppy, t)
      = Complex(gcf_fused(_REAL, suppx, suppy, overc(_U, t) + OVER * overc(_V, t)),
                gcf_fused(_IMAG, suppx, suppy, overc(_U, t) + OVER * overc(_V, t)));

  // ** Definition

  // Reduction domain. Note that we iterate over time steps before
  // switching the GCF row in order to increase locality (Romein).
  RDom red(
      0, _CPLX_FIELDS
    , 0, GCF_SIZE
    , vis.top(), vis.height()
    , 0, GCF_SIZE
    );
  RVar
      rcmplx = red.x
    , rgcfx = red.y
    , rvis  = red.z
    , rgcfy = red.w
    ;

  // Get visibility as complex number
  Complex visC(vis(_R, rvis), vis(_I, rvis));

  // Update grid
  uvg(rcmplx,
      rgcfx + clamp(uv(_U, rvis), 0, grid_size - 1 - GCF_SIZE),
      rgcfy + clamp(uv(_V, rvis), 0, grid_size - 1 - GCF_SIZE))
    += select(inBound(rvis),
              (visC * Complex(gcf(rgcfx, rgcfy, rvis))).unpack(rcmplx),
              cast<double>(0.0f));

  // ** Strategy

  // Compute UV & oversampling coordinates per visibility
  overc.compute_at(uvg, rvis).vectorize(uvdim);
  uv.compute_at(uvg,rvis).vectorize(uvdim);
  inBound.compute_at(uvg,rvis);

  // Fuse and vectorise complex calculations of entire GCF rows
  RVar rgcfxc;
  uvg.update()
    .allow_race_conditions()
    .fuse(rgcfx, rcmplx, rgcfxc)
    .vectorize(rgcfxc, 8)
    .unroll(rgcfxc, GCF_SIZE * 2 / 8);

  Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});
  Module mod = uvg.compile_to_module(args, "kern_scatter", target);
  compile_module_to_object(mod, argv[1]);
  return 0;
}
