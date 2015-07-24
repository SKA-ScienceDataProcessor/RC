#include "GCF.h"

#include <omp.h>
#define _USE_MATH_DEFINES
#include <cmath>
#include <cstring>
#ifdef _MSC_VER
#include <vector>
#endif

#include "metrix.h"
#include "fft_dyn_padded.h"

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

  src += (max_support / 2 - support / 2) * over * (src_pitch + 1);
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

// In principle, we can calculate 1 - (t2/r)^2 separately
//   because it does not depend on w and can be reused for
//   all layers, but we don't bother with caching and copying them
//   over and thus recalculate them each time
template <int over>
fftw_plan __mkGCFLayer(
    fftw_plan p
  , complexd dst[]   // [over][over][support][support]
  , complexd * table[]
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
  // #pragma omp parallel for // WRONG USAGE!
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

  fftw_plan plan = fft_inplace_even(p, FFTW_FORWARD, arena, size, pitch);

  __transpose_and_normalize_and_extract<over>(
      dst
    , arena
    , support
    , max_support
    , src_pad
    );

  // Fill layers table
  complexd * tp = dst;
  for (int i=0; i < over * over; i++) {
    table[i] = tp;
    tp += support * support;
  }

  return plan;
}

// Inst
fftw_plan mkGCFLayer(
    fftw_plan p
  , complexd dst[] // should have [OVER][OVER][support][support] size
  , complexd * table[]
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
      , table
      , arena
      , support
      , max_support
      , src_pad
      , t2
      , w
      );
}

// This function is required to
//   correctly calculate GCF, namely we
//   need to know the correct w's mean value
//   for each w-plane.
void calcAccums(
    const Double3 uvw[]
  // We retain separate sum and num of points info
  //   because it is a valuable information
  , /*out*/ double sums[]
  , /*out*/ int npts[]
  , double wstep
  , int numDataPoints // baselines * channels * timesteps
  , int numOfWPlanes
  ) {
  int nthreads;
  int center = numOfWPlanes / 2;

#pragma omp parallel
#pragma omp single
  nthreads = omp_get_num_threads();

  memset(sums, 0, sizeof(double) * numOfWPlanes);
  memset(npts, 0, sizeof(int) * numOfWPlanes);

#ifndef _MSC_VER
  // VLAs. Requires GNU extension.
  double tmpsums[nthreads-1][numOfWPlanes];
  int tmpnpts[nthreads-1][numOfWPlanes];
#else
  using namespace std;
  vector<vector<double>> tmpsums(nthreads-1, vector<double>(numOfWPlanes));
  vector<vector<int>> tmpnpts(nthreads-1, vector<int>(numOfWPlanes));
#endif

  #pragma omp parallel
  {
    int _thread = omp_get_thread_num();
    double * _sums;
    int * _npts;
    if (_thread == 0) {
      _sums = sums;
      _npts = npts;
    } else {
#ifndef _MSC_VER
      _sums = tmpsums[_thread - 1];
      _npts = tmpnpts[_thread - 1];
#else
      _sums = tmpsums[_thread - 1].data();
      _npts = tmpnpts[_thread - 1].data();
#endif
    }
    memset(_sums, 0, sizeof(double) * numOfWPlanes);
    memset(_npts, 0, sizeof(int) * numOfWPlanes);

    // Center _sums and _npts
    _sums += center;
    _npts += center;
    double w;
    int wplane;
    #pragma omp for schedule(dynamic)
    for(int n = 0; n < numDataPoints; n++) {
      w = uvw[n].w / wstep;
      wplane = int(round(w));
      _sums[wplane] += uvw[n].w;
      _npts[wplane]++;
    }
  }
  // Simple linear addition (it is faster than starting any threads here)
  // Don't use any fancy AVX tricks because of
  // a negligibility of the whole processing time.
  for(int i=0; i<nthreads-1; i++)
  for(int j=0; j<numOfWPlanes; j++) {
    sums[j] += tmpsums[i][j];
    npts[j] += tmpnpts[i][j];
  }
}
