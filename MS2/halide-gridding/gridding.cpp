#include "Halide.h"
#include <stdio.h>

using namespace Halide;

int main(int argc, char **argv) {
    ImageParam UVW(Float(64), 2);    // second dimension is used for U, V and W parts. So it should be Nx3 sized.

    // Constant supersample factor.
    Expr supersampleFactor = 8;

    // The Omega - telescope view angle.
    Expr Omega = pi/2;

    // Two two dimensional images for prolate spheroidal wave function.
    // For real and imaginary parts.
    ImageParam PSWFReal(Float(64),2);
    ImageParam PSWFImag(Float(64),2);

    // Iterator within UVW triples.
    RDom uvwRange (0, UVW.width());

    // fetch the values.
    Expr U = UVW(uvwRange.x,0);
    Expr V = UVW(uvwRange.x,1);
    Expr W = UVW(uvwRange.x,2);

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

    // The 

#if 0
    Param<float> r_sigma("r_sigma");
    int s_sigma = atoi(argv[1]);
    Var x("x"), y("y"), z("z"), c("c");

    // Add a boundary condition
    Func clamped("clamped");
    clamped(x, y) = input(clamp(x, 0, input.width()-1),
                          clamp(y, 0, input.height()-1));

    // Construct the bilateral grid
    RDom r(0, s_sigma, 0, s_sigma);
    Expr val = clamped(x * s_sigma + r.x - s_sigma/2, y * s_sigma + r.y - s_sigma/2);
    val = clamp(val, 0.0f, 1.0f);
    Expr zi = cast<int>(val * (1.0f/r_sigma) + 0.5f);
    Func histogram("histogram");
    histogram(x, y, z, c) = 0.0f;
    histogram(x, y, zi, c) += select(c == 0, val, 1.0f);

    // Blur the grid using a five-tap filter
    Func blurx("blurx"), blury("blury"), blurz("blurz");
    blurz(x, y, z, c) = (histogram(x, y, z-2, c) +
                         histogram(x, y, z-1, c)*4 +
                         histogram(x, y, z  , c)*6 +
                         histogram(x, y, z+1, c)*4 +
                         histogram(x, y, z+2, c));
    blurx(x, y, z, c) = (blurz(x-2, y, z, c) +
                         blurz(x-1, y, z, c)*4 +
                         blurz(x  , y, z, c)*6 +
                         blurz(x+1, y, z, c)*4 +
                         blurz(x+2, y, z, c));
    blury(x, y, z, c) = (blurx(x, y-2, z, c) +
                         blurx(x, y-1, z, c)*4 +
                         blurx(x, y  , z, c)*6 +
                         blurx(x, y+1, z, c)*4 +
                         blurx(x, y+2, z, c));

    // Take trilinear samples to compute the output
    val = clamp(input(x, y), 0.0f, 1.0f);
    Expr zv = val * (1.0f/r_sigma);
    zi = cast<int>(zv);
    Expr zf = zv - zi;
    Expr xf = cast<float>(x % s_sigma) / s_sigma;
    Expr yf = cast<float>(y % s_sigma) / s_sigma;
    Expr xi = x/s_sigma;
    Expr yi = y/s_sigma;
    Func interpolated("interpolated");
    interpolated(x, y, c) =
        lerp(lerp(lerp(blury(xi, yi, zi, c), blury(xi+1, yi, zi, c), xf),
                  lerp(blury(xi, yi+1, zi, c), blury(xi+1, yi+1, zi, c), xf), yf),
             lerp(lerp(blury(xi, yi, zi+1, c), blury(xi+1, yi, zi+1, c), xf),
                  lerp(blury(xi, yi+1, zi+1, c), blury(xi+1, yi+1, zi+1, c), xf), yf), zf);

    // Normalize
    Func bilateral_grid("bilateral_grid");
    bilateral_grid(x, y) = interpolated(x, y, 0)/interpolated(x, y, 1);

    Target target = get_target_from_environment();
    if (target.has_gpu_feature()) {
        histogram.compute_root().reorder(c, z, x, y).gpu_tile(x, y, 8, 8);
        histogram.update().reorder(c, r.x, r.y, x, y).gpu_tile(x, y, 8, 8).unroll(c);
        blurx.compute_root().gpu_tile(x, y, z, 16, 16, 1);
        blury.compute_root().gpu_tile(x, y, z, 16, 16, 1);
        blurz.compute_root().gpu_tile(x, y, z, 8, 8, 4);
        bilateral_grid.compute_root().gpu_tile(x, y, s_sigma, s_sigma);
    } else {

        // CPU schedule
        histogram.compute_at(blurz, y);
        histogram.update().reorder(c, r.x, r.y, x, y).unroll(c);
        blurz.compute_root().reorder(c, z, x, y).parallel(y).vectorize(x, 4).unroll(c);
        blurx.compute_root().reorder(c, x, y, z).parallel(z).vectorize(x, 4).unroll(c);
        blury.compute_root().reorder(c, x, y, z).parallel(z).vectorize(x, 4).unroll(c);
        bilateral_grid.compute_root().parallel(y).vectorize(x, 4);
    }
    
    bilateral_grid.compile_to_file("bilateral_grid", r_sigma, input, target);
#endif

    return 0;
}



