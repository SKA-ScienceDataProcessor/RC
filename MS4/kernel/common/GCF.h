#ifndef __GCF_H
#define __GCF_H

#include <fftw3.h>
#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

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
  );

void calcAccums(
    const Double3 uvw[]
  // We retain separate sum and num of points info
  //   because it is a valuable information
  , /*out*/ double sums[]
  , /*out*/ int npts[]
  , double wstep
  , int numDataPoints // baselines * channels * timesteps
  , int numOfWPlanes
  );

#ifdef __cplusplus
}
#endif

#endif
