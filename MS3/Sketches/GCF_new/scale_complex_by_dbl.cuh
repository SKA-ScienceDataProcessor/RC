#ifndef __SCALE_COMPLEX_BY_DBL_CUH
#define __SCALE_COMPLEX_BY_DBL_CUH

#include <cuComplex.h>

__device__ static __inline__ cuDoubleComplex cuMulComplexByDouble(cuDoubleComplex v,
                                                             double y){
  return make_cuDoubleComplex ( v.x * y
                              , v.y * y
                              );
}

__device__ __inline__
void scale_complex_by_dbl(
    double norm
  , cuDoubleComplex * v
  , int len
  ) {
  const int x = blockIdx.x * blockDim.x + threadIdx.x;
  if (x < len) v[x] = cuMulComplexByDouble(v[x], norm);
}

#endif
