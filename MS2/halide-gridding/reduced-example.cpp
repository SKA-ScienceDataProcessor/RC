// Gridding algoithm, adopted from bilateral_grid demo app.
// There's nothing left from bilateral_grid except ideas and typical Halide idioms.

#include <math.h>
#include "Halide.h"
#include <stdio.h>

using namespace Halide;

int main(int argc, char **argv) {

    ImageParam Coords(Float(64), 2);

    // Iterator within UVW triples.
    RDom coordsRange (0, Coords.extent(0));

    Func spritePixel("spritePixel");

    Var spriteCoordIndex("spriteCoordIndex"), spriteImageIndex("spriteImageIndex");

    Expr spriteWidth("spriteWidth"), spriteCoord("spriteCoord"), spriteZ("spriteZ");

    spriteZ = Coords(spriteCoordIndex, 1);
    spriteCoord = Coords(spriteCoordIndex, 0);

    spriteWidth = cast<int>(sqrt(1+spriteZ*spriteZ));

    spritePixel(spriteCoordIndex, spriteImageIndex) =
        select(spriteImageIndex >= spriteCoord-spriteWidth
               && spriteImageIndex <= spriteCoord+spriteWidth,
               1/sqrt(1+spriteZ*spriteZ+(spriteCoord-spriteImageIndex)*(spriteCoord-spriteImageIndex)),
               cast<double>(0.0));

    Func result("result");

    Var x("x");

    Expr y("y");
    y = coordsRange.x;

    result(x) = cast<double>(0.0);
    result(x) += spritePixel(y, x);

    result.update().reorder(x, coordsRange.x);

    Target compile_target = get_target_from_environment();
    std::vector<Halide::Argument> compile_args;
    compile_args.push_back(Coords);
    result.compile_to_c("reduced-example.c", compile_args);

    return 0;
}