#include "Halide.h"
#include <stdio.h>
using namespace Halide;

int main(int argc, char **argv) {
    if (argc < 2) return 1;
    Func gen;
    Var  x;

    gen(x) = cast<float>(0);

    gen.vectorize(x,8);
    gen.output_buffer().set_stride(0,1);

    std::vector<Argument> args = {};
    Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41,});
    compile_module_to_object(gen.compile_to_module(args, "kern_recovery", target), argv[1]);
    return 0;
}
