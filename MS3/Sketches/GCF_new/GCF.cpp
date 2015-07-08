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


template <int over>
void __transpose_and_extract(
    complexd dst[]       // [over][over][support][support]
  , const complexd src[] // [max_support][over][max_support][over]
  , int support
  , int max_support
  , int src_pad
  ) {
  int
      src_size = max_support * over
    , src_pitch = src_size + src_pad
    , start_loc = (max_support - support) * over * src_pitch / 2
    ;
  const complexd * srcp = src + start_loc;

  // NOTE: check this thoroughly
  for (int overu = 0; overu < over; overu++, srcp+=src_pitch) {
    const complexd * srcp1; srcp1 = srcp;
    for (int overv = 0; overv < over; overv++, srcp1++) {
      const complexd * srcp2; srcp2 = srcp1;
      for (int suppu = 0; suppu < support; suppu++, srcp2+=over*src_pitch) {
        const complexd * srcp3; srcp3 = srcp2;
        for (int suppv = 0; suppv < support; suppv++, srcp3+=over) {
          *dst++ = *srcp3;
        }
      }
    }
  }
}

// Inst
extern "C"
void transpose_and_extract(
    complexd dst[]
  , const complexd src[]
  , int support
  , int max_support
  , int src_pad
  ) {
  __transpose_and_extract<8>(dst, src, support, max_support, src_pad);
}
