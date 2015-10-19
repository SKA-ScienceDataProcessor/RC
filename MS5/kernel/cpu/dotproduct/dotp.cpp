// Kernel for calculation of dot product
#include "Halide.h"
#include <stdio.h>
using namespace Halide;

int main(int argc, char **argv) {
    if (argc < 2) return 1;
    Var  x;
    Func dotp;

    ImageParam vec_f(type_of<float>(), 1);
    ImageParam vec_g(type_of<float>(), 1);

    dotp(x) = vec_f(x) * vec_g(x);

    vec_f.set_stride(0,1);
    vec_g.set_stride(0,1);
    dotp.output_buffer().set_stride(0,1);
    dotp.vectorize(x,8);

    std::vector<Argument> args = {vec_f, vec_g};
    Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});
    compile_module_to_object(dotp.compile_to_module(args, "kern_dotp", target), argv[1]);
    return 0;
}
