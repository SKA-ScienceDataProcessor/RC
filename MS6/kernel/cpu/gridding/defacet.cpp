
#include "Halide.h"
using namespace Halide;

int main(int argc, char **argv) {
    if (argc < 2) return 1;

    ImageParam facet(type_of<double>(), 4, "facet");

    Func image("image"); Var x("x"),y("y");
    image(x,y) = undef<double>();

    // Update with data
    RDom rdom(facet.min(0), facet.extent(0),
              facet.min(1), facet.extent(1),
              facet.min(2), facet.extent(2),
              facet.min(3), facet.extent(3));
    RVar rx = rdom.x, ry = rdom.y, rl = rdom.z, rm = rdom.w;
    image(clamp(rx+rl, image.output_buffer().left(), image.output_buffer().right()),
          clamp(ry+rm, image.output_buffer().left(), image.output_buffer().right()))
       = facet(rx,ry,rl,rm);

    std::vector<Halide::Argument> args = { facet };
    Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});
    Module mod = image.compile_to_module(args, "kern_defacet", target);
    compile_module_to_object(mod, argv[1]);
    return 0;
}
