// Gridding algoithm, adopted from bilateral_grid demo app.
// There's nothing left from bilateral_grid except ideas and typical Halide idioms.

#include "Halide.h"
#include <stdio.h>

using namespace Halide;

int main(int argc, char **argv) {
    ImageParam UVW(Float(64), 4);    // second dimension is used for U, V and W parts. So it should be Nx3 sized.

    // Constant supersample factor.
    Expr supersampleFactor = 8;

    // The Omega - telescope view angle.
    Expr Omega = pi/2;

    // Two dimensional image for prolate spheroidal wave function. Last dimension is fixed to 2.
    ImageParam PSWF(Float(64),3);

    Var pswfKernelHalf("pswfKernelHalf");

    // pswf kernel image height and width are odd, 2N+1. To recover the half, and not to overflow image bounds,
    // we first subtract 1 and only then divide by 2. It have to work for 2N+1 and is safe for even values.
    // (albeit incorrect).
    pswfKernelHalf = (PSWF.extent(0)-1) / 2;

    // Iterator within UVW triples.
    RDom uvwRange (0, UVW.extent(0), 0, UVW.extent(1), 0, UVW.extent(2));

    Var timestep("timestep");
    Var baseline("baseline");
    Var channel("channel");

    baseline = uvwRange.x;
    timestep = uvmRange.y;
    timestep = uvwRange.z;

    // fetch the values.
    Var U("U");
    U = UVW(baseline, timestep, channel, 0);
    Var V("V");
    V = UVW(baseline, timestep, channel, 1);
    Var W("W");
    W = UVW(baseline, timestep, channel, 2);

    Expr intU = cast<int>U;
    Expr intV = case<int>V;

    Expr supersampleU = cast<int>(supersampleFactor*(U - intU));
    Expr supersampleV = cast<int>(supersampleFactor*(V - intV));

    // Computation of kernel width.
    Expr onePercent = 0.01;
    Expr wOmegaBy2 = W*Omega/2;
    Expr GKernelWidth = sqrt(wOmegaBy2*(wOmegaBy2 + pi*onePercent/sqrt(w)));
    Expr GKernelWidthInt = cast<int>(GKernelWidth+0.5);

    // Range over kernel space. It is two dimensional, to account for U and V.
    RDom GUVRange (-GKernelWidthInt, GKernelWidthInt+1, -GKernelWidthInt, GKernelWidthInt+1);

    Var GU("GU");
    Var GV("GV");

    GU = GUVRange.x;
    GV = GUVRange.y;

    // Range over PSWF space.
    RDom pswfRange(-pswfKernelHalf, pswfKernelHalf+1, -pswfKernelHalf, pswfKernelHalf+1);

    Var PSWFU("PSWFU");
    Var PSWFV("PSWFV");

    PSWFU = pswfRange.x;
    PSWFV = pswfRange.y;

    // The G kernel.
    Func G("G");

    // !!! XXX FIXME !!! XXX FIXME
    G(GU,GV) = 1-G*0.01-U*0.01;

    // The computation of the result.
    Func gridding("gridding");
    var rU("rU");
    Var rV("rV");

    // computing the result
    rU = intU + GU+PSWFU;
    rU = intV + GV+PSWFV;

    gridding(rU, rV) = 0.0;
    gridding(rU, rV) += PSWF(PSWFU, PSWFV)*G(GU,GV); // This product can be cached. We also can drop assignment to zero and only update image.

    Target compile_target = get_target_from_environment();
    gridding.compile_to_file("gridding_compiled", UVW, PSWF, compile_target);

    return 0;
}



