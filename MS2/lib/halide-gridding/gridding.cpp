// Gridding algoithm, adopted from bilateral_grid demo app.
// There's nothing left from bilateral_grid except ideas and typical Halide idioms.

#include <math.h>
#include "Halide.h"
#include <stdio.h>

using namespace Halide;

template<typename T>
void gridding_func_simple(std::string typeName) {
    int Tbits = sizeof(T) * 8;
    ImageParam UVW(Halide::type_of<T>(), 3, "UVW");     // baseline, timestep, UVW triples.
    ImageParam visibilities(Halide::type_of<T>(), 3, "visibilities");   // baseline, timestep, polarization fuzed with complex number.
    ImageParam support(Halide::type_of<T>(), 4, "supportSimple");      // baseline, u,v and two values of complex number.
    ImageParam supportSize(Int(32), 1, "supportSize");

    RDom uvwRange (0, UVW.extent(0), 0, UVW.extent(1));

    Var baseline("baseline"), timestep("timestep");

    // fetch the values.
    Func U("U");
    U(baseline, timestep) = UVW(baseline, timestep, 0);
    Func V("V");
    V(baseline, timestep) = UVW(baseline, timestep, 1);

    Func intU;
    intU(baseline, timestep) = cast<int>(U(baseline, timestep));
    Func intV;
    intV(baseline, timestep) = cast<int>(V(baseline, timestep));

    Func supportWidth("supportWidth");
    Var x("x");
    supportWidth(x) = supportSize(x);

    Func supportWidthHalf("supportWidthHalf");
    supportWidthHalf(x) = supportWidth(x)/2;

    std::string resultName = "griddingSimple_"+typeName;

    Func result(resultName);

    // the weight of support changes with method - method here is SIMPLE..
    Func weightr("weightr"), weighti("weighti");;
    Var weightBaseline("weightBaseline"), cu("cu"), u("u"), cv("cv"), v("v");
    weightr(weightBaseline, cu, cv, u, v) =
        select(abs(cv-v) <= supportWidthHalf(weightBaseline) && abs(cu-u) <= supportWidthHalf(weightBaseline),
                support(weightBaseline,
                        clamp(u-cu+supportWidthHalf(weightBaseline), 0, support.extent(1)-1),
                        clamp(v-cv+supportWidthHalf(weightBaseline), 0, support.extent(2)-1), 0),
                (T)0.0);
    weighti(weightBaseline, cu, cv, u, v) =
        select(abs(cv-v) <= supportWidthHalf(weightBaseline) && abs(cu-u) <= supportWidthHalf(weightBaseline),
                support(weightBaseline,
                        clamp(u-cu+supportWidthHalf(weightBaseline), 0, support.extent(1)-1),
                        clamp(v-cv+supportWidthHalf(weightBaseline), 0, support.extent(2)-1), 1),
                (T)0.0);

    RDom polarizations(0,4);

    Var polarization("polarization");

    Func visibilityr("silibilityr"), visibilityi("visibilityi");
    visibilityr(baseline, timestep, polarization) = visibilities(baseline, timestep, polarization*2);
    visibilityi(baseline, timestep, polarization) = visibilities(baseline, timestep, polarization*2+1);

    Var pol("pol");

    Expr baselineR("baselineR"), timestepR("timestepR");

    baselineR = uvwRange.x;
    timestepR = uvwRange.y;

    // four dimensional result.
    // dimensions are: u, v, polarizations (XX,XY,YX, and YY, four total) and real and imaginary values of complex number.
    // so you should reaalize result as this: result.realize(MAX_U, MAX_V, 4, 2);
    result(u, v, pol, x)  = (T)0.0;
    result(u, v, pol, 0) +=
          weightr(baselineR, intU(baselineR, timestepR), intV(baselineR, timestepR),u,v)
        * visibilityr(baselineR, timestepR, pol);
    result(u, v, pol, 1) +=
          weighti(baselineR, intU(baselineR, timestepR), intV(baselineR, timestepR),u,v)
        * visibilityi(baselineR, timestepR, pol);

    // no optimizations: 483K FLOPS

#if 0
    // This reordering boosts to 727K FLOPS
    result.update().reorder(u,v, uvwRange.x, uvwRange.y);

    // // tile the U and V into small window that fits the cache. Performance is 727K FLOPS
    // Var Uinner, Uouter, Vinner, Vouter;
    // result.tile(u, v, Uouter, Vouter, Uinner, Vinner, 16, 16);

    // // vectorizing Vinner. Performance  717K FLOPS
    // result.vectorize(Vinner);

    // Different split and vectorization combined with unrolling. 716K FLOPS
    // For now I consider this futile.
    Var VPolFused, VPolComplexFused, Uinner, Uouter, Vinner, Vouter, UVTileIndex;
    result
        .fuse(v, pol, VPolFused)
        .fuse(VPolFused, x, VPolComplexFused)
        .tile(u, VPolComplexFused, Uouter, Vouter, Uinner, Vinner, 4, 64)
        .unroll(Uinner)
        .vectorize(Vinner)
        .parallel(Uouter);
#else
    Var uOuter, uInner;
    result
        .parallel(u, 64);
    result.update(0)
        .parallel(u, 64);
    result.update(1)
        .parallel(u, 64);
#endif

    Target compile_target = get_target_from_environment();
    std::vector<Halide::Argument> compile_args;
    compile_args.push_back(UVW);
    compile_args.push_back(visibilities);
    compile_args.push_back(support);
    compile_args.push_back(supportSize);
    result.compile_to_file(resultName, compile_args);
//    result.compile_to_c(resultName+".cpp", compile_args);

} /* gridding_func_simple */

int main(int argc, char **argv) {
    gridding_func_simple<float>("float");

    return 0;
}



