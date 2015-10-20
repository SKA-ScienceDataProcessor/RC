
#include "Halide.h"
using namespace Halide;

int main(int argc, char **argv) {
    if (argc < 2) return 1;

    Func grid; Var cmplx,x,y;
    grid(cmplx,x,y) = cast<double>(0.0f);

    // No need for a strategy - LLVM should automatically figure out
    // that this is a memset.

    Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});
    compile_module_to_object(grid.compile_to_module({}, "kern_init", target), argv[1]);
    return 0;
}
