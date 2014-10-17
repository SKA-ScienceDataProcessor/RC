// Gridding algoithm, adopted from bilateral_grid demo app.
// There's nothing left from bilateral_grid except ideas and typical Halide idioms.

#include <math.h>
#include "Halide.h"
#include <stdio.h>

using namespace Halide;

template<typename T>
void gridding_func_simple(std::string typeName) {
    int Tbits = sizeof(T) * 8;
    ImageParam UVW(Float(Tbits), 4, "UVW");
    ImageParam visibilities(Float(Tbits), 4, "visibilities");   // baseline, channel, timestep, polarization fuzed withcomplex number.
    ImageParam support(Float (Tbits), 4, "supportSimple");      // baseline, u,v and two values of complex number.
    ImageParam supportSize(Int(32), 1, "supportSize");

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

    Expr supportWidth("supportWidth");
    supportWidth = supportSize(baseline);

    Expr supportWidthHalf("supportWidthHalf");
    supportWidthHalf = supportWidth/2;

    RDom convRange(-supportWidthHalf, supportWidth, -supportWidthHalf, supportWidth);

    Func result("result");

    // the weight of support changes with method.
    Expr weightr("weightr"), weighti("weighti");;
    weightr = support(baseline, convRange.x+supportWidthHalf, convRange.y+supportWidthHalf, 0);
    weighti = support(baseline, convRange.x+supportWidthHalf, convRange.y+supportWidthHalf, 1);

    RDom polarizations(0,4);

    Expr visibilityr("silibilityr"), visibilityi("visibilityi");
    visibilityr = visibilities(baseline, timestep, channel,polarizations.x*2);
    visibilityi = visibilities(baseline, timestep, channel,polarizations.x*2);

    Var u("u"), v("v"), pol("pol");

    result(u, v, pol, 0)  = 0.0;
    result(u, v, pol, 1)  = 0.0;
    result(u, v, pol, 0) += select(u > 0, weightr*visibilityr, 0.0);;
    result(u, v, pol, 1) += select(u > 0, weighti*visibilityi, 0.0);;

    Target compile_target = get_target_from_environment();
    std::vector<Halide::Argument> compile_args;
    compile_args.push_back(UVW);
    compile_args.push_back(visibilities);
    compile_args.push_back(support);
    compile_args.push_back(supportSize);
    result.compile_to_c("gridding_compiled.c", compile_args);

} /* gridding_func_simple */

int main(int argc, char **argv) {
    gridding_func_simple<float>("float");

    return 0;
}



