/* mk_baseline_wplane_map.cpp

  Copyright (C) 2015 Braam Research, LLC.
 */

#include <omp.h>
#include <string.h>

#include "common.h"
#include "OskarBinReader.h"

void mkBlWpMap(const WMaxMin * bl_ws, int numBaselines, double wstep, BlWMap * bl_wis)
{
  double cmax, cmin;
  for (int i = 0; i < numBaselines; i++) {
    cmax = bl_ws[i].maxw/wstep;
    cmin = bl_ws[i].minw/wstep;
    bl_wis[i].bl = i;
    /*
    if (cmax > 0.0)
      bl_wis[i].wp = round(cmax);
    else if (cmin < 0.0)
      bl_wis[i].wp = round(cmin);
    else
      bl_wis[i].wp = 0;
     */
    // Use minimum instead of maximum
    // to make things easier at the moment,
    // because this determines max support handled by
    // threadblock in Romein-like algorithm and if it
    // is greater than that determined by current data point
    // we point to nonsensical GCF data.
    if (cmax < 0.0)
      bl_wis[i].wp = int(round(cmax));
    else if (cmin > 0.0)
      bl_wis[i].wp = int(round(cmin));
    else
      bl_wis[i].wp = 0;
  }
}

long long count_points(BlWMap * m, int n){
  long long res = 0;
  for(int i = 0; i < n; i++) {
    int supp = get_supp(m[i].wp);
    res += supp * supp;
  }
  return res;
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

#pragma omp parallel
#pragma omp single
  nthreads = omp_get_num_threads();

  memset(sums, 0, sizeof(double) * numOfWPlanes);
  memset(npts, 0, sizeof(int) * numOfWPlanes);

  // VLAs. Requires GNU extension.
  double tmpsums[nthreads-1][numOfWPlanes];
  int tmpnpts[nthreads-1][numOfWPlanes];

  #pragma omp parallel
  {
    int _thread = omp_get_thread_num();
    double * _sums;
    int * _npts;
    if (_thread == 0) {
      _sums = sums;
      _npts = npts;
    } else {
      _sums = tmpsums[_thread - 1];
      _npts = tmpnpts[_thread - 1];
    }
    memset(_sums, 0, sizeof(double) * numOfWPlanes);
    memset(_npts, 0, sizeof(int) * numOfWPlanes);

    double w;
    int wplane;
    #pragma omp for schedule(dynamic)
    for(int n = 0; n < numDataPoints; n++) {
      w = uvw[n].w / wstep;
      wplane = int(round(w));
      _sums[wplane] += w;
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
