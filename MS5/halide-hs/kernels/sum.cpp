// Kernel for calculation of dot product
#include "Halide.h"
#include <stdio.h>
using namespace Halide;

int main(int argc, char **argv) {
    Var  x;
    Func sum;

       
    ImageParam vec(type_of<float>(), 1);

    RDom r(vec.min(0), vec.extent(0));
    sum()  = cast<float>(0);
    sum() += vec(r);

    std::vector<Argument> args = {vec};
    sum.compile_to_file("kern_sum", args);
    return 0;
}

