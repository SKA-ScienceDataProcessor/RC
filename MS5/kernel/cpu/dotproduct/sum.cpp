// Kernel for calculation of dot product
#include "Halide.h"
#include <stdio.h>
using namespace Halide;

int main(int argc, char **argv) {
    if (argc < 2) return 1;
    Var  x;
    Func sum;

    ImageParam vec(type_of<float>(), 1);

    RDom r(vec.min(0), vec.extent(0));
    sum()  = cast<float>(0);
    sum() += vec(r);

    vec.set_stride(0,1);

    // Doesn't work because it can't prove that the vector extend is
    // divisible by 8...
    //sum.update().allow_race_conditions().vectorize(r.x,8);

    std::vector<Argument> args = {vec};
    Target target(Target::OSUnknown, Target::X86, 64, { Target::SSE41, Target::AVX});
    compile_module_to_object(sum.compile_to_module(args, "kern_sum", target), argv[1]);
    return 0;
}
