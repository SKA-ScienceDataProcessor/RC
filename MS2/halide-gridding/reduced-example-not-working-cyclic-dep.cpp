// Gridding algoithm, adopted from bilateral_grid demo app.
// There's nothing left from bilateral_grid except ideas and typical Halide idioms.

#include <math.h>
#include "Halide.h"
#include <stdio.h>

using namespace Halide;

int main(int argc, char **argv) {

    ImageParam Coords(Float(64), 2);

    // Iterator within UVW triples.
    Expr coordIndex("coordIndex");

    // fetch the values.
    Expr Coord("Coord"), Z("Z");


    RDom combinedRange(0, Coords.extent(0), -Width, 2*Width+1);          // the sprite size.

    coordIndex = combinedRange.x;

    Coord = Coords(coordIndex, 0);
    Z = Coords(coordIndex, 1);

    Expr Width = cast<int>(sqrt(1+Z*Z));        // just an example.

    Expr CenterX = cast<int>(Coord);

    Expr low = CenterX - Width;
    Expr high = CenterX + Width;

    Var x;
    Var z;

    Func sprite("sprite");

//    sprite(x) = 1/sqrt(1+Z*Z+x*x);
//    sprite(x) = 1/sqrt(1+x*x);

    Func result("result");

    result(x) = cast<double>(0.0);
//    result(CenterX+spriteRange.x) += sprite(Coord-CenterX+spriteRange.x);
#define sqr(x) ((x)*(x))
    result(CenterX+combinedRange.y) += 1/(sqr(Coord-CenterX+combinedRange.y)+sqr(Z));
   // result(x) += select(x>= low && x <= high, 1/sqrt(sqr(Coord-x)+sqr(Z)), 0);

    Target compile_target = get_target_from_environment();
    std::vector<Halide::Argument> compile_args;
    compile_args.push_back(Coords);
    result.compile_to_c("reduced-example.c", compile_args);

    return 0;
}



