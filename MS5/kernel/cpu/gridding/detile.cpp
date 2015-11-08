
#include "Halide.h"
using namespace Halide;

enum ComplxFields { _REAL = 0, _IMAG,  _CPLX_FIELDS };

int main(int argc, char **argv) {
    if (argc < 2) return 1;

    ImageParam in_grid(type_of<double>(), 3, "in_grid");
    in_grid.set_extent(0, _CPLX_FIELDS).set_stride(1, _CPLX_FIELDS);

    Func out_grid("out_grid"); Var cmplx("c"),x("x"),y("y");
    out_grid(cmplx,x,y) = undef<double>();

    // Determine bounds
    OutputImageParam out_buf = out_grid.output_buffer();
    Expr xmin = max(out_buf.min(1), in_grid.min(1)),
         xmax = min(out_buf.min(1) + out_buf.extent(1),
                    in_grid.min(1) + in_grid.extent(1));
    Expr ymin = max(out_buf.min(2), in_grid.min(2)),
         ymax = min(out_buf.min(2) + out_buf.extent(2),
                    in_grid.min(2) + in_grid.extent(2));

    // Update with data
    RDom rdom(0,2,xmin,xmax-xmin,ymin,ymax-ymin);
    RVar rc = rdom.x, rx = rdom.y, ry = rdom.z;
    out_grid(rc,rx,ry) += in_grid(rc,rx,ry);

    RVar rcx("cx");
    out_grid.output_buffer().set_extent(0, _CPLX_FIELDS).set_stride(1, _CPLX_FIELDS);
    out_grid.bound(cmplx, 0, _CPLX_FIELDS);
    out_grid.update().fuse(rx, rc, rcx).vectorize(rcx, 2);

    std::vector<Halide::Argument> args = { in_grid };
    Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});
    Module mod = out_grid.compile_to_module(args, "kern_detile", target);
    compile_module_to_object(mod, argv[1]);
    return 0;
}
