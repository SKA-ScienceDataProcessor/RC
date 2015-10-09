// Kernel for calculation of dot product
#include "Halide.h"
#include <stdio.h>
using namespace Halide;

int main(int argc, char **argv) {
    if (argc < 2) return 1;
    Var  x;
    Func dotp;

    Param<int32_t> start("start", 0);
    ImageParam vec_f(type_of<float>(), 1);
    ImageParam vec_g(type_of<float>(), 1);

    dotp(x) = vec_f(x) * vec_g(x);

    std::vector<Argument> args = {start, vec_f, vec_g};
    compile_module_to_object(dotp.compile_to_module(args, "kern_dotp"), argv[1]);
    return 0;
}
