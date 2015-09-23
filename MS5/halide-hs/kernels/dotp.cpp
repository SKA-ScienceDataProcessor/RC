// Kernel for calculation of dot product
#include "Halide.h"
#include <stdio.h>
using namespace Halide;

int main(int argc, char **argv) {
    Var  x;
    Func dotp;

    ImageParam vec_f(type_of<float>(), 1);
    ImageParam vec_g(type_of<float>(), 1);

    dotp(x) = vec_f(x) * vec_g(x);

    std::vector<Argument> args = {vec_f, vec_g};
    dotp.compile_to_file("kern_dotp", args);
    return 0;
}
