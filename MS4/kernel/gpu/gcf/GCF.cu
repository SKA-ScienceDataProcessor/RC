#include <cuda.h>
#include <cuComplex.h>
#include <math_constants.h>

#define __SET_MAP                               \
  const int                                     \
      x = blockIdx.x * blockDim.x + threadIdx.x \
    , y = blockIdx.y * blockDim.y + threadIdx.y \
    ;

template <int max_support>
__device__ __inline__
void ucs_common(
    cuDoubleComplex mesh[max_support][max_support]
  , double t2
  ){
  __SET_MAP
  const int max_half_support = max_support / 2;
  double
      t2_div_sc = t2 / double(max_half_support)
    , xs = double(x - max_half_support) * t2_div_sc
    , ys = double(y - max_half_support) * t2_div_sc
    ;
  mesh[x][y] = make_cuDoubleComplex(xs * xs + ys * ys, 0.0);
}

extern "C" __global__ void r2(cuDoubleComplex mesh[256][256], double t2) {
  ucs_common<256>(mesh, t2);
}


template <int max_support>
__device__ __inline__
void calc(
    cuDoubleComplex dst[max_support][max_support]
  , const cuDoubleComplex src[max_support][max_support]
  , double w
  ){
  __SET_MAP
  double ph = w * (1.0 - sqrt(1.0 - src[x][y].x));
  double s, c;
  sincos(2.0 * CUDART_PI * ph, &s, &c);

  dst[x][y] = make_cuDoubleComplex(c, -s); // to get rid of conj later
}

extern "C" __global__ void wkernff(
    cuDoubleComplex dst[256][256]
  , const cuDoubleComplex src[256][256]
  , double w
  ){
  calc<256>(dst, src, w);
}


template <
    int max_support
  , int oversample
  >
__device__ __inline__
void copy_ucs_2_over(
    cuDoubleComplex dst[max_support * oversample][max_support * oversample]
  , const cuDoubleComplex src[max_support][max_support]
  ){
  __SET_MAP
  const int max_half_support = max_support / 2;
  const int dst_center = max_support * oversample / 2 - max_half_support;
  dst[dst_center + x][dst_center + y] = src[x][y];
}

extern "C" __global__  void copy_2_over(
    cuDoubleComplex dst[2048][2048]
  , const cuDoubleComplex src[256][256]
  ){
  copy_ucs_2_over<256,8>(dst, src);
}


// We use 3rd grid dimension to cover oversample range
template <
    int max_support
  , int oversample
  >
__device__ __inline__
void transpose_over(
    cuDoubleComplex dst[oversample][oversample][max_support][max_support]
  , const cuDoubleComplex src[max_support * oversample][max_support * oversample]
  ) {
  __SET_MAP
  const int
      overx = blockIdx.z / oversample
    , overy = blockIdx.z % oversample
    , sx = x * oversample + overx
    , sy = y * oversample + overy
    ;
  dst[overx][overy][x][y] = src[sx][sy];
}

extern "C" __global__ void transpose_over0(
    cuDoubleComplex dst[8][8][256][256]
  , const cuDoubleComplex src[2048][2048]
  ){
  transpose_over<256,8>(dst, src);
}


template <
    int max_support
  , int oversample
  >
__device__ __inline__
void cut_out(
    int half_supp
  , cuDoubleComplex * dst
  , const cuDoubleComplex src[max_support][max_support]
  ) {
  __SET_MAP
  const int supp = half_supp * 2 + 1;

  if (x >= supp || y >= supp) return;

  dst[x * supp + y] = src[x][y];
}

extern "C" __global__ void wextract1(
    int half_supp
  , cuDoubleComplex * dst
  , const cuDoubleComplex src[256][256]
  ){
  cut_out<256,8>(half_supp, dst, src);
}
