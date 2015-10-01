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

    std::vector<Argument> args = {vec};
    compile_module_to_object(sum.compile_to_module(args, "kern_sum"), argv[1]);
    return 0;
}

