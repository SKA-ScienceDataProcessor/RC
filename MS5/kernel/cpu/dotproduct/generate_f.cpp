// Kernel for generation of vector
#include "Halide.h"
#include <stdio.h>
using namespace Halide;

int main(int argc, char **argv) {
    if (argc < 2) return 1;
    Func gen;
    Var  x;

    Param<int32_t> start("start", 0);

    gen(x) = cast<float>(start + x);

    std::vector<Argument> args = {start};
    compile_module_to_object(gen.compile_to_module(args, "kern_generate_f"), argv[1]);
    return 0;
}
