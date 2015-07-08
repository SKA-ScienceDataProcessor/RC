#include "common.h"
#include <cmath>

using namespace std;

extern "C"
void fft_inplace_even(complexd * data, int size, int pitch);

// In principle, we can calculate 1 - (t2/r)^2 separately
//   because it does not depend on w and can be reused for
//   all layers, but we don't bother with caching and copying them
//   over and thus recalculate them each time
void mkGCFLayer(
    complexd arena[] // Full oversampled layer (padded)
  , int size         // linear size of the arena
  , int pitch        // pitch of the arena
  , double t2
  , double w
  , int max_support
  ){
  acc2d<complexd> center = acc2d<complexd>(arena + size * pitch / 2, pitch);

  int radius = max_support / 2;
  double normer = t2 / double (radius);

  #pragma omp parallel for // not so much important
  // Can make this slightly more optimal ...
  for(int i = 0; i <= radius; i++)
  for(int j = 0; i <= radius; i++) {
    double x, y, ph;
    x = double(i) / normer;
    y = double(j) / normer;
    ph = w * (1 - sqrt(1 - x*x - y*y));
    double s, c;
    sincos(2.0 * M_PI * ph, &s, &c);
      center[-i][-j]
    = center[-i][ j]
    = center[ i][-j]
    = center[ i][ j] = complexd(c,-s);
  }
  fft_inplace_even(arena, size, pitch);
  // TODO:
  // Transposition and extraction here.
  // I believe we should combine transposition with extraction.
}
