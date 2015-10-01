// Kernel for generation of vector
#include "Halide.h"
#include <stdio.h>
using namespace Halide;

int main(int argc, char **argv) {
    if (argc < 2) return 1;
    Func gen;
    Var  x;

    gen(x) = cast<float>(x);

    std::vector<Argument> args = {};
    printf("%s\n", argv[1]);
    compile_module_to_object(gen.compile_to_module(args, "kern_generate_f"), argv[1]);
    return 0;
}
