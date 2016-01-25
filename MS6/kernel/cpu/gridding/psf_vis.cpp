
#include "Halide.h"
#include "utils.h"
using namespace Halide;

int main(int argc, char **argv) {
  if (argc < 2) return 1;

  // Visibilities: Array of 5-pairs, packed together with UVW
  enum VisFields { _U=0, _V, _W, _R, _I,  _VIS_FIELDS };
  ImageParam vis(type_of<double>(), 2, "vis");
  vis.set_min(0,0).set_stride(0,1).set_extent(0,_VIS_FIELDS)
     .set_stride(1,_VIS_FIELDS);
  std::vector<Halide::Argument> args = { vis };

  // All we want to do is set the visibility to 1.0+0.0j
  Func psfVis("psfVis"); Var uvdim("uvdim"), t("t");
  psfVis(uvdim, t) = select(uvdim == _R, cast<double>(1.0f),
                            uvdim == _I, cast<double>(0.0f),
                            vis(uvdim, t));
  psfVis.unroll(uvdim);
  psfVis.output_buffer()
     .set_min(0,0).set_stride(0,1).set_extent(0,_VIS_FIELDS)
     .set_stride(1,_VIS_FIELDS);

  Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});
  Module mod = psfVis.compile_to_module(args, "kern_psf_vis", target);
  compile_module_to_object(mod, argv[1]);
  return 0;
}
