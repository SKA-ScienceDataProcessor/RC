
#include "Halide.h"

const int CYCLES = 12;

using namespace Halide;

enum Coordinates {X, Y, val};

struct Hogbom {
    Hogbom();
    ImageParam res, psf;
    Param<double> gain;
    Param<double> threshold;
    Expr pPeakx, pPeaky, pPeakVal;

    Var x, y;

    Func psfPeak;
    Func psfBounded;
    Func findPeak[CYCLES];
    Func residual[CYCLES+1];
    Func residual_out;
    Func underThreshold[CYCLES+1];
    Func model;

    void minorCycle(int i);
};

Hogbom::Hogbom()
    : x("x"), y("y")
    , psfPeak("psfPeak")
    , psfBounded("psfBounded")
    , residual_out("residual_out")
    , model("model")
{

    // Set function names. Halide will add numbered prefixes.
    for(int i = 0; i < CYCLES+1; i++) {
        if (i < CYCLES) findPeak[i] = Func("findPeak");
        residual[i] = Func("residual");
        underThreshold[i] = Func("underThreshold");
    }

    // Initialise inputs
    res = ImageParam(type_of<double>(), 2, "res");
    psf = ImageParam(type_of<double>(), 2, "psf");
    psfBounded = BoundaryConditions::constant_exterior(psf, cast<double>(0.0f));

    // Leave outputs as they are coming in
    residual_out(x,y) = undef<double>();
    model(x,y) = cast<double>(0);
    residual[0](x,y) = res(x,y);
    underThreshold[0]() = cast<bool>(0);

    // Find psf peaks
    RDom rdp(psf);
    psfPeak() = argmax(rdp, abs(res(rdp.x, rdp.y)), "psfPeak");
    pPeakx = psfPeak()[X];
    pPeaky = psfPeak()[Y];
    pPeakVal = psfPeak()[val];

    // Do minor cycles
    for(int i = 0; i < CYCLES; i++) {
        // Do minor cycle
        minorCycle(i);
        // Copy out residual if we are under threshold
        residual_out(x,y) = select(!underThreshold[i] && underThreshold[i+1],
                                   residual[i](x,y), undef<double>());
    }

    // Copy out last residual if threshold hasn't been breached yet
    residual_out(x,y) = select(!underThreshold[CYCLES],
                               residual[CYCLES](x,y), undef<double>());

}

void Hogbom::minorCycle(int i)
{

    // Find peak and new peak val. Note that this updates findPeak(),
    // which changes the behaviour of ifOverThreshold below!
    RDom rdr(res);
    findPeak[i]()
        = argmax(rdr, abs(select(underThreshold[i](),
                                 undef<double>(),
                                 residual[i](rdr.x, rdr.y))),
                 "findPeakAm");
    Expr peakX = findPeak[i]()[X], peakY = findPeak[i]()[Y],
         peakVal = findPeak[i]()[val];
    Expr clampedX = clamp(peakX, 0, res.extent(X)-1);
    Expr clampedY = clamp(peakY, 0, res.extent(Y)-1);

    // Under threshold?
    underThreshold[i+1]() = print(underThreshold[i]() || peakVal < threshold,
                                  "peak found: ", peakX, peakY, peakVal);

    // Calculate how much we need to shift the PSF, remove from residual
    Expr diffx = clamp(peakX - pPeakx, -psf.extent(X), res.extent(X)),
         diffy = clamp(peakY - pPeaky, -psf.extent(Y), res.extent(Y));
    residual[i+1](x, y) = select(underThreshold[i+1](), undef<double>(),
                                 residual[i](x, y) -
                                 gain * peakVal / pPeakVal *
                                 psfBounded(clamp(x - diffx, -1, psf.extent(X)+1),
                                            clamp(y - diffy, -1, psf.extent(Y)+1)));

    // Add pixel in model
    model(clampedX, clampedY) += select(underThreshold[i+1](), undef<double>(), gain * peakVal);
}

int main(int argc, char **argv)
{
    if (argc < 2) return 1;

    // Create hogbom, with some half-way sensible update order
    Hogbom c;
    c.psfPeak.compute_root();
    for(int i = 0; i < CYCLES; i++) {
        c.findPeak[i].compute_root();
        c.underThreshold[i+1].compute_root();
        c.residual[i+1].compute_root();
    }

    std::vector<Halide::Argument> args = {c.gain, c.threshold, c.psf, c.res};
    Target target(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});

    std::vector<Module> ms = {
        c.model.compile_to_module(args, "kern_hogbom_model", target),
        c.residual[CYCLES+1].compile_to_module(args, "kern_hogbom_residual", target)
    };
    compile_module_to_object(link_modules("kern_hogbom", ms), argv[1]);
    return 0;
}
