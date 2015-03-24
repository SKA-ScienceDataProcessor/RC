#include <cuda.h>
#include <cuComplex.h>
#include <math_constants.h>

// Use symmetry
// Use max_half_support threads only
// Perhaps it's not very cache friendly, but it is
// very simple to perform work-distribution for this variant

template <int max_half_support>
__device__
void ucs_common(
    cuDoubleComplex mesh[max_half_support * 2 + 1][max_half_support * 2 + 1]
  , double t2
  ){
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y
    ;
  double
      sc = double(max_half_support)
    , xs = double(x) / sc * t2
    , ys = double(y) / sc * t2
    ;
  cuDoubleComplex r2 = make_cuDoubleComplex(xs * xs + ys * ys, 0.0);

  mesh[max_half_support - x][max_half_support - y] = r2;
  mesh[max_half_support - x][max_half_support + y] = r2;
  mesh[max_half_support + x][max_half_support - y] = r2;
  mesh[max_half_support + x][max_half_support + y] = r2;
}

// test instantiation
template __device__
void ucs_common<256>(cuDoubleComplex mesh[513][513], double t2);


template <int max_half_support>
__device__
void calc_inplace(
    cuDoubleComplex mesh[max_half_support * 2 + 1][max_half_support * 2 + 1]
  , double w
  ){
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y
    ;
  // mesh is symmetric, thus we need no recalc ph
  double ph = w * (1.0 - sqrt(1.0 - mesh[max_half_support - x][max_half_support - y].x));
  double s, c;
  sincos(2.0 * CUDART_PI * ph, &s, &c);

  cuDoubleComplex res = make_cuDoubleComplex(c, -s); // to get rid of conj later

  mesh[max_half_support - x][max_half_support - y] = res;
  mesh[max_half_support - x][max_half_support + y] = res;
  mesh[max_half_support + x][max_half_support - y] = res;
  mesh[max_half_support + x][max_half_support + y] = res;
}

// test instantiation
template __device__
void calc_inplace<256>(
    cuDoubleComplex mesh[513][513]
  , double w
  );


template <int max_half_support>
__device__
void calc(
    cuDoubleComplex dst[max_half_support * 2 + 1][max_half_support * 2 + 1]
  , const cuDoubleComplex src[max_half_support * 2 + 1][max_half_support * 2 + 1]
  , double w
  ){
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y
    ;
  double ph = w * (1.0 - sqrt(1.0 - src[max_half_support - x][max_half_support - y].x));
  double s, c;
  sincos(2.0 * CUDART_PI * ph, &s, &c);

  cuDoubleComplex res = make_cuDoubleComplex(c, -s); // to get rid of conj later

  dst[max_half_support - x][max_half_support - y] = res;
  dst[max_half_support - x][max_half_support + y] = res;
  dst[max_half_support + x][max_half_support - y] = res;
  dst[max_half_support + x][max_half_support + y] = res;
}

// test instantiation
template __device__
void calc<256>(
    cuDoubleComplex dst[513][513]
  , const cuDoubleComplex src[513][513]
  , double w
  );


template <
    int max_half_support
  , int oversample
  >
__device__
void copy_ucs_2_over(
    cuDoubleComplex dst[max_half_support * oversample * 2 + 1][max_half_support * oversample * 2 + 1]
  , const cuDoubleComplex src[max_half_support * 2 + 1][max_half_support * 2 + 1]
  ) {
  const int
      dst_center = max_half_support * oversample
    , pad = dst_center - max_half_support
#ifndef __SET_NULL_PADDING
    , cut = dst_center + max_half_support
#endif
    ;
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y
    ;
#ifndef __SET_NULL_PADDING
  if (
       x < pad
    || x > cut
    || y < pad
    || y > cut
    ) dst[x][y] = {0.0, 0.0};
  else
#endif
      dst[x][y] = src[x - pad][y - pad];
  }

// test instantiation
template __device__
void copy_ucs_2_over<256,8>(
    cuDoubleComplex dst[4097][4097]
  , const cuDoubleComplex src[513][513]
  );


template <
    int max_half_support
  , int oversample
  >
__device__
void cut_out(
    int supp
  , cuDoubleComplex * dst
  , const cuDoubleComplex src[max_half_support * oversample * 2 + 1][max_half_support * oversample * 2 + 1]
  ) {
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y
    ;
  const int off = (max_half_support - supp) * oversample;

  dst[x * supp + y] = src[off+x][off+y];
  }

// test instantiation
template __device__
void cut_out<256,8>(
    int supp
  , cuDoubleComplex * dst
  , const cuDoubleComplex src[4097][4097]
  );
