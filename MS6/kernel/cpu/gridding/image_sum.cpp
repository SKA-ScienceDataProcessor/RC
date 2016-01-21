
#include "Halide.h"
using namespace Halide;

int main(int argc, char **argv) {
    if (argc < 2) return 1;

    ImageParam image(type_of<double>(), 4, "image");

    Func sum("sum"); Var x("x"),y("y");
    sum(x,y) = undef<double>();

    // Update with data
    RDom rdom(image.min(0), image.extent(0),
              image.min(1), image.extent(1),
              image.min(2), image.extent(2),
              image.min(3), image.extent(3));
    RVar rx = rdom.x, ry = rdom.y, rl = rdom.z, rm = rdom.w;
    sum(clamp(rx+rl, sum.output_buffer().left(), sum.output_buffer().right()),
        clamp(ry+rm, sum.output_buffer().left(), sum.output_buffer().right()))
       += image(rx,ry,rl,rm);

    std::vector<Halide::Argument> args = { image };
    Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});
    Module mod = sum.compile_to_module(args, "kern_image_sum", target);
    compile_module_to_object(mod, argv[1]);
    return 0;
}
