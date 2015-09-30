// Kernel for generation of vector
#include "Halide.h"
#include <stdio.h>
using namespace Halide;

int main(int argc, char **argv) {
    Func gen;
    Var  x;

    gen(x) = cast<float>(x)/10;
    
    std::vector<Argument> args = {};
    gen.compile_to_file("kern_generate_f", args);
    return 0;
}
