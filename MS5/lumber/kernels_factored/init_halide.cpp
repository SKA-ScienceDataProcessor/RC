#include "kernels_halide.h"

FullFunc genInit() {
    Func grid; Var cmplx,x,y;
    grid(cmplx,x,y) = cast<double>(0.0f);
    return make_pair(std::vector<Argument>(), grid);
}
