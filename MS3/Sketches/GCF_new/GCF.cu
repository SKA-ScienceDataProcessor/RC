#include <cuda.h>
#include <cuComplex.h>
#include <math_constants.h>

template <int max_half_support>
__device__
void ucs_common(cuDoubleComplex mesh[max_half_support * 2 + 1][max_half_support * 2 + 1]){
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y
    ;
  double
      sc = double(max_half_support)
    , xs = double(x - max_half_support) / sc
    , ys = double(y - max_half_support) / sc
    ;

  mesh[x][y].x = xs * xs + ys * ys;
  mesh[x][y].x = 0.0;
}


// test instantiation
template __device__
void ucs_common<256>(cuDoubleComplex mesh[513][513]);

template <int max_half_support>
__device__
void calc_inplace(
    cuDoubleComplex mesh[max_half_support * 2 + 1][max_half_support * 2 + 1]
  , double t2
  , double w
  ){
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y
    ;
  double
      r2 = mesh[x][y].x * t2 * t2
    , ph = w * (1.0 - sqrt(1.0 - r2))
    ;
  double s, c;
  sincos(2.0 * CUDART_PI * ph, &s, &c);
  mesh[x][y].x = s;
  mesh[x][y].y = c;
}

// test instantiation
template __device__
void calc_inplace<256>(
    cuDoubleComplex mesh[513][513]
  , double t2
  , double w
  );

template <int max_half_support>
__device__
void calc(
    cuDoubleComplex dst[max_half_support * 2 + 1][max_half_support * 2 + 1]
  , cuDoubleComplex src[max_half_support * 2 + 1][max_half_support * 2 + 1]
  , double t2
  , double w
  ){
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y
    ;
  double
      r2 = src[x][y].x * t2 * t2
    , ph = w * (1.0 - sqrt(1.0 - r2))
    ;
  double s, c;
  sincos(2.0 * CUDART_PI * ph, &s, &c);
  dst[x][y].x = s;
  dst[x][y].y = c;
}

// test instantiation
template __device__
void calc<256>(
    cuDoubleComplex dst[513][513]
  , cuDoubleComplex src[513][513]
  , double t2
  , double w
  );

template <
    int max_half_support
  , int oversample
  >
__device__
void copy_ucs_2_over(
    cuDoubleComplex dst[max_half_support * oversample * 2 + 1][max_half_support * oversample * 2 + 1]
  , cuDoubleComplex src[max_half_support * 2 + 1][max_half_support * 2 + 1]
  ) {
  const int
      dst_center = max_half_support * oversample
    , pad = dst_center - max_half_support
    , cut = dst_center + max_half_support
    ;
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y
    ;
  if (
       x < pad
    || x > cut
    || y < pad
    || y > cut
    ) dst[x][y] = {0.0, 0.0};
  else
      dst[x][y] = src[x - pad][y - pad];
  }

// test instantiation
template __device__
void copy_ucs_2_over<256,8>(
    cuDoubleComplex dst[4097][4097]
  , cuDoubleComplex src[513][513]
  );
