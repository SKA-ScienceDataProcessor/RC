#ifdef _MSC_VER
#pragma warning(push, 0)
#endif
#include "Halide.h"
#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include "utils.h"

using namespace Halide;

std::string mkKernelName(const std::string & prefix, int GCF_SIZE){
  return prefix + "_" + std::to_string(GCF_SIZE);
}

Module scatterKernel(Target target, int GCF_SIZE) {

  // Oversampling is a constant for now
  const int OVER = 8;

  // ** Input

  Param<double> scale("scale");
  Param<int> grid_size("grid_size");
  Param<int> margin_size("margin_size");

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

  std::vector<Halide::Argument> args = { scale, grid_size, margin_size, vis, gcf_fused };

  // ** Output

  // Grid starts out undefined so we can update the output buffer
  Func uvg("uvg");
  Var cmplx("cmplx"), x("x"), y("y");
  uvg(cmplx, x, y) = undef<double>();

  uvg.output_buffer()
    .set_stride(0,1).set_extent(0,_CPLX_FIELDS)
    .set_stride(1,_CPLX_FIELDS);

  // Get grid limits. This limits the uv pixel coordinates we accept
  // for the top-left corner of the GCF.
  Expr gcf_margin = max(0, (margin_size - GCF_SIZE) / 2);
  Expr min_u = uvg.output_buffer().min(1) + gcf_margin;
  Expr max_u = uvg.output_buffer().min(1) + uvg.output_buffer().extent(1) - GCF_SIZE - 1 - gcf_margin;
  Expr min_v = uvg.output_buffer().min(2) + gcf_margin;
  Expr max_v = uvg.output_buffer().min(2) + uvg.output_buffer().extent(2) - GCF_SIZE - 1 - gcf_margin;

  // ** Helpers

  // Coordinate preprocessing
  Func uvs("uvs"), uv("uv"), overc("overc");
  Var uvdim("uvdim"), t("t");
  uvs(uvdim, t) = vis(uvdim, t) * scale;
  overc(uvdim, t) = clamp(cast<int>(round(OVER * (uvs(uvdim, t) - floor(uvs(uvdim, t))))), 0, OVER-1);
  uv(uvdim, t) = cast<int>(round(uvs(uvdim, t)) + grid_size / 2 - GCF_SIZE / 2);

  // Visibilities to ignore due to being out of bounds
  Func inBound("inBound");
  inBound(t) = uv(_U, t) >= min_u && uv(_U, t) <= max_u &&
               uv(_V, t) >= min_v && uv(_V, t) <= max_v;

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
      rgcfx + clamp(uv(_U, rvis), min_u, max_u),
      rgcfy + clamp(uv(_V, rvis), min_v, max_v))
    += select(inBound(rvis),
              (visC * Complex(gcf(rgcfx, rgcfy, rvis))).unpack(rcmplx),
              undef<double>());

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

  return uvg.compile_to_module(args, mkKernelName("kern_scatter", GCF_SIZE), target);
}

int main(int argc, char **argv)
{
    if (argc < 2) return 1;
    Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX });
    std::vector<Module> modules =
      { scatterKernel(target,  8)
      , scatterKernel(target, 16)
      , scatterKernel(target, 32)
      , scatterKernel(target, 64)
      };
    Module linked = link_modules("kern_scatters", modules);
    compile_module_to_c_header(linked, std::string(argv[1]) + ".h");
    compile_module_to_object(linked, argv[1]);
    return 0;
}
