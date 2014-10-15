// Gridding algoithm, adopted from bilateral_grid demo app.
// There's nothing left from bilateral_grid except ideas and typical Halide idioms.

#include <math.h>
#include "Halide.h"
#include <stdio.h>

using namespace Halide;

int main(int argc, char **argv) {

    double pi_const = acos(-1);
    Expr pi("pi");
    pi = cast<double>(pi_const);

    ImageParam UVW(Float(64), 4);    // second dimension is used for U, V and W parts. So it should be Nx3 sized.

    // Constant supersample factor.
    Expr supersampleFactor = 8;

    // The Omega - telescope view angle.
    Expr Omega = cast<double>(pi/2);

    // Two dimensional image for prolate spheroidal wave function. Last dimension is fixed to 2.
    ImageParam PSWF(Float(64),3);

    Expr pswfKernelHalf("pswfKernelHalf");

    // pswf kernel image height and width are odd, 2N+1. To recover the half, and not to overflow image bounds,
    // we first subtract 1 and only then divide by 2. It have to work for 2N+1 and is safe for even values.
    // (albeit incorrect).
    pswfKernelHalf = (PSWF.extent(0)-1) / 2;

    // Iterator within UVW triples.
    RDom uvwRange (0, UVW.extent(0), 0, UVW.extent(1), 0, UVW.extent(2));

    Expr timestep("timestep");
    Expr baseline("baseline");
    Expr channel("channel");

    baseline = uvwRange.x;
    timestep = uvwRange.y;
    channel  = uvwRange.z;

    // fetch the values.
    Expr U("U");
    U = UVW(baseline, timestep, channel, 0);
    Expr V("V");
    V = UVW(baseline, timestep, channel, 1);
    Expr W("W");
    W = UVW(baseline, timestep, channel, 2);

    Expr intU = cast<int>(U);
    Expr intV = cast<int>(V);

    Expr supersampleU = cast<int>(supersampleFactor*(U - intU));
    Expr supersampleV = cast<int>(supersampleFactor*(V - intV));

    // Computation of kernel width.
    Expr onePercent = cast<double>(0.01);
    Expr wOmegaBy2 = W*Omega/2;
    Expr GKernelWidth = sqrt(wOmegaBy2*(wOmegaBy2 + cast<double>(pi)*cast<double>(onePercent)/sqrt(W)));
    Expr GKernelWidthInt = cast<int>(GKernelWidth+cast<double>(0.5));

    // Range over kernel space. It is two dimensional, to account for U and V.
    RDom GUVRange (-GKernelWidthInt, GKernelWidthInt+1, -GKernelWidthInt, GKernelWidthInt+1);

    Expr GU("GU");
    Expr GV("GV");

    GU = GUVRange.x;
    GV = GUVRange.y;

    // Range over PSWF space.
    RDom pswfRange(-pswfKernelHalf, pswfKernelHalf+1, -pswfKernelHalf, pswfKernelHalf+1);

    Expr PSWFU("PSWFU");
    Expr PSWFV("PSWFV");

    PSWFU = pswfRange.x;
    PSWFV = pswfRange.y;

    // The expression of G.
    Func G("GExpr");
    Var u("u"), v("v");
    G(u,v) = 1 - u*0.01 - v*0.01;

    // The computation of the result.
    Func gridding("gridding");
    Var rU("rU");
    Var rV("rV");

    // computing the result
    //rU = intU + GU+PSWFU;
    //rU = intV + GV+PSWFV;

    RDom griddingPoint (intU-GKernelWidthInt, intU+GKernelWidthInt+1, intV - GKernelWidthInt, intV+GKernelWidthInt+1);

    gridding(rU, rV) = 0.0;
    gridding(griddingPoint.x, griddingPoint.y) += G(griddingPoint.x-intU, griddingPoint.y-intV); //PSWF(pswfRange.x, pswfRange.y)*G(GUVRange.x,GUVRange.y); // This product can be cached. We also can drop assignment to zero and only update image.

    Target compile_target = get_target_from_environment();
    std::vector<Halide::Argument> compile_args;
    compile_args.push_back(UVW);
    gridding.compile_to_c("gridding_compiled.c", compile_args);

    return 0;
}



