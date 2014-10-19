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

    Var baseline("baseline"), timestep("timestep"), channel("channel");

    // fetch the values.
    Func U("U");
    U(baseline, timestep, channel) = UVW(baseline, timestep, channel, 0);
    Func V("V");
    V(baseline, timestep, channel) = UVW(baseline, timestep, channel, 1);
    Func W("W");
    W(baseline, timestep, channel) = UVW(baseline, timestep, channel, 2);

    Func intU;
    intU(baseline, timestep, channel) = cast<int>(U(timestep, baseline, channel));
    Func intV;
    intV(baseline, timestep, channel) = cast<int>(V(timestep, baseline, channel));

    Func supportWidth("supportWidth");
    Var x("x");
    supportWidth(x) = supportSize(x);

    Func supportWidthHalf("supportWidthHalf");
    supportWidthHalf(x) = supportWidth(x)/2;

    RDom convRange(-supportWidthHalf, supportWidth, -supportWidthHalf, supportWidth);

    Func result("result");

    // the weight of support changes with method - method here is SIMPLE..
    Func weightr("weightr"), weighti("weighti");;
    Var weightBaseline("weightBaseline"), cu("cu"), u("u"), cv("cv"), v("v");
    weightr(weightBaseline, cu, cv, u, v) =
        select(abs(cv-v) <= supportWidth(weightBaseline) && abs(cu-u) <= supportWidth(weightBaseline),
                support(weightBaseline,
                        clamp(cu-u+supportWidth(weightBaseline)/2, 0, support.extent(1)),
                        clamp(cv-v+supportWidth(weightBaseline)/2, 0, support.extent(2)), 0),
                (T)0.0);
    weighti(weightBaseline, cu, cv, u, v) =
        select(abs(cv-v) <= supportWidth(weightBaseline) && abs(cu-u) <= supportWidth(weightBaseline),
                support(weightBaseline,
                        clamp(cu-u+supportWidth(weightBaseline)/2, 0, support.extent(1)),
                        clamp(cv-v+supportWidth(weightBaseline)/2, 0, support.extent(2)), 1),
                (T)0.0);

    RDom polarizations(0,4);

    Var polarization("polarization");

    Func visibilityr("silibilityr"), visibilityi("visibilityi");
    visibilityr(baseline, timestep, channel, polarization) = visibilities(baseline, timestep, channel,polarization*2);
    visibilityi(baseline, timestep, channel, polarization) = visibilities(baseline, timestep, channel,polarization*2+1);

    Var pol("pol");

    Expr baselineR("baselineR"), timestepR("timestepR"), channelR("channelR");

    baselineR = uvwRange.x;
    timestepR = uvwRange.y;
    channelR = uvwRange.z;

    // four dimensional result.
    // dimensions are: u, v, polarizations (XX,XY,YX, and YY, four total) and real and imaginary values of complex number.
    // so you should reaalize result as this: result.realize(MAX_U, MAX_V, 4, 2);
    result(u, v, pol, x)  = (T)0.0;
    result(u, v, pol, 0) += weightr(baselineR, intU(baselineR, timestepR, channelR), intV(baselineR, timestepR, channelR),u,v) *visibilityr(baselineR, timestepR, channelR, pol);
//    result(u, v, pol, 1) += select(u > 0, weighti*visibilityi, 0.0);;

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



