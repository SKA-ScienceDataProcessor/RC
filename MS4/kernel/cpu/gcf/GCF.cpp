#define _USE_MATH_DEFINES
#include <cmath>
#include <fftw3.h>

#include "common.h"
#include "metrix.h"

using namespace std;

template <int over>
void __transpose_and_normalize_and_extract(
    complexd dst[]       // [over][over][support][support]
  , const complexd src[] // [max_support][over][max_support][over]
  , int support
  , int max_support
  , int src_pad
  ) {

  double sums[over][over];  for(int i = 0; i<over*over; i++) (&sums[0][0])[i] = 0.0;

  const complexd * srcp = src;
  for (int suppu = 0; suppu < max_support; suppu++)
  for (int overu = 0; overu < over; overu++)
  for (int suppv = 0; suppv < max_support; suppv++)
  for (int overv = 0; overv < over; overv++)
     sums[overu][overv] += (srcp++)->real();

  int
      src_size = max_support * over
    , src_pitch = src_size + src_pad
    ;

  for (int overu = 0; overu < over; overu++, src+=src_pitch) {
    const complexd * srcp1; srcp1 = src;
    for (int overv = 0; overv < over; overv++, srcp1++) {
      const complexd * srcp2; srcp2 = srcp1;
      for (int suppu = 0; suppu < support; suppu++, srcp2+=over*src_pitch) {
        const complexd * srcp3; srcp3 = srcp2;
        for (int suppv = 0; suppv < support; suppv++, srcp3+=over) {
          *dst++ = *srcp3 / sums[overu][overv];
        }
      }
    }
  }
}

extern "C"
fftw_plan fft_inplace_even(fftw_plan p, complexd * data, int size, int pitch);

// In principle, we can calculate 1 - (t2/r)^2 separately
//   because it does not depend on w and can be reused for
//   all layers, but we don't bother with caching and copying them
//   over and thus recalculate them each time
template <int over>
fftw_plan __mkGCFLayer(
    fftw_plan p
  , complexd dst[]   // [over][over][support][support]
  , complexd arena[] // Full oversampled layer padded [max_support*over][max_support*over+src_pad]
  , int support
  , int max_support
  , int src_pad      // padding value
  , double t2
  , double w
  ){
  int
      size = max_support * over
    , pitch = size + src_pad
    ;

  int radius = max_support / 2;
  double normer = t2 / double (radius);

  complexd *currp = arena + (pitch+1) * (size/2-radius);
  int
      radiuspos = radius + max_support % 2
    , istep = pitch - max_support
    ;

  memset(arena, 0, size * pitch * sizeof(complexd));
  #pragma omp parallel for // not so much important
  for(int i = -radius; i < radiuspos; i++) {
    for(int j = -radius; j < radiuspos; j++) {
      double x, y, ph;
      x = double(i) * normer;
      y = double(j) * normer;
      ph = w * (1 - sqrt(1 - x*x - y*y));
      double s, c;
      sincos(2.0 * M_PI * ph, &s, &c);
      *currp++ = complexd(c,-s);
    }
    currp += istep;
  }
  fftw_plan plan = fft_inplace_even(p, arena, size, pitch);
  __transpose_and_normalize_and_extract<over>(
      dst
    , arena
    , support
    , max_support
    , src_pad
    );
  return plan;
}

// Inst
extern "C"
fftw_plan mkGCFLayer(
    fftw_plan p
  , complexd dst[] // should have [OVER][OVER][support][support] size
  , complexd arena[]
  , int support
  , int max_support
  , int src_pad
  , double t2
  , double w
  ){
  return __mkGCFLayer<OVER>(
        p
      , dst
      , arena
      , support
      , max_support
      , src_pad
      , t2
      , w
      );
}
