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

Module degridKernel(Target target, int GCF_SIZE) {
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

  // Get grid limits. This limits the uv pixel coordinates we accept
  // for the top-left corner of the GCF.
  ImageParam uvg(type_of<double>(), 3, "uvg");
  uvg.set_stride(0,1).set_extent(0,_CPLX_FIELDS)
     .set_stride(1,_CPLX_FIELDS);
  Expr gcf_margin = max(0, (margin_size - GCF_SIZE) / 2);
  Expr min_u = uvg.min(1) + gcf_margin;
  Expr max_u = uvg.min(1) + uvg.extent(1) - GCF_SIZE - 1 - gcf_margin;
  Expr min_v = uvg.min(2) + gcf_margin;
  Expr max_v = uvg.min(2) + uvg.extent(2) - GCF_SIZE - 1 - gcf_margin;

  std::vector<Halide::Argument> args = { scale, grid_size, margin_size, gcf_fused, uvg, vis };

  // ** Helpers

  // Coordinate preprocessing
  Func uvs("uvs"), uv("uv"), overc("overc");
  Var uvdim("uvdim"), tdim("tdim3");
  uvs(uvdim, tdim) = vis(uvdim, tdim) * scale;
  overc(uvdim, tdim) = clamp(cast<int>(round(OVER * (uvs(uvdim, tdim) - floor(uvs(uvdim, tdim))))), 0, OVER-1);
  uv(uvdim, tdim) = cast<int>(round(uvs(uvdim, tdim)) + grid_size / 2 - GCF_SIZE / 2);

  // Visibilities to ignore due to being out of bounds
  Func inBound("inBound");
  inBound(tdim) =
    uv(_U, tdim) >= min_u && uv(_U, tdim) <= max_u &&
    uv(_V, tdim) >= min_v && uv(_V, tdim) <= max_v;

  // GCF lookup for a given visibility
  Func gcf("gcf");
  Var suppx("suppx"), suppy("suppy"), overx("overx"), overy("overy");
  gcf(suppx, suppy, tdim)
      = Complex(gcf_fused(_REAL, suppx, suppy, overc(_U, tdim) + OVER * overc(_V, tdim)),
                gcf_fused(_IMAG, suppx, suppy, overc(_U, tdim) + OVER * overc(_V, tdim)));

  // ** Output

  // We cannot change "vis" as "uv" depends on it, so we have to make
  // a copy.
  Func vis_out("vis_out");
  vis_out(uvdim, tdim) = vis(uvdim, tdim);
  vis_out.bound(uvdim, 0, _VIS_FIELDS);

  // Reduction domain.
  RDom red(
     _R, 2
    , 0, GCF_SIZE
    , 0, GCF_SIZE);
  RVar rcmplx = red.x, rgcfx = red.y, rgcfy = red.z;

  // Subtract visibilites in-place
  Expr u = rgcfx + clamp(uv(_U, tdim), min_u, max_u);
  Expr v = rgcfy + clamp(uv(_V, tdim), min_v, max_v);
  vis_out(rcmplx, tdim) -=
      select(inBound(tdim),
             (Complex(uvg(_REAL, u, v), uvg(_IMAG, u, v)) *
              Complex(gcf(rgcfx, rgcfy, tdim))).unpack(rcmplx-_R),
             undef<double>());

  // Compute UV & oversampling coordinates per visibility
  overc.compute_at(vis_out, tdim);
  uv.compute_at(vis_out, tdim);
  inBound.compute_at(vis_out, tdim);
  vis_out.unroll(uvdim);
  vis_out.update().unroll(rcmplx);

  return vis_out.compile_to_module(args, mkKernelName("kern_degrid", GCF_SIZE), target);
}

int main(int argc, char **argv)
{
    if (argc < 2) return 1;
    Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX });
    std::vector<Module> modules =
      { degridKernel(target, 16)
      , degridKernel(target, 32)
      , degridKernel(target, 64)
      };
    Module linked = link_modules("kern_degrids", modules);
    compile_module_to_c_header(linked, std::string(argv[1]) + ".h");
    compile_module_to_object(linked, argv[1]);
    return 0;
}
