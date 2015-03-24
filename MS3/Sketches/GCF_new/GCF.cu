#include <cuda.h>
#include <cuComplex.h>
#include <math_constants.h>

// Use symmetry
// Use max_half_support threads only
// Perhaps it's not very cache friendly, but it is
// very simple to perform work-distribution for this variant

#define __SET_MAP                               \
  const int                                     \
      x = blockIdx.x * blockDim.x + threadIdx.x \
    , y = blockIdx.y * blockDim.y + threadIdx.y \
    , xl = max_half_support - x                 \
    , xr = max_half_support + x                 \
    , yl = max_half_support - y                 \
    , yr = max_half_support + y                 \
    ;


template <int max_half_support>
__device__
void ucs_common(
    cuDoubleComplex mesh[max_half_support * 2 + 1][max_half_support * 2 + 1]
  , double t2
  ){
  __SET_MAP
  double
      sc = double(max_half_support)
    , xs = double(x) / sc * t2
    , ys = double(y) / sc * t2
    ;
  cuDoubleComplex r2 = make_cuDoubleComplex(xs * xs + ys * ys, 0.0);

  mesh[xl][yl] = r2;
  mesh[xl][yr] = r2;
  mesh[xr][yl] = r2;
  mesh[xr][yr] = r2;
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
  __SET_MAP
  // mesh is symmetric, thus we need no recalc ph
  double ph = w * (1.0 - sqrt(1.0 - mesh[xl][yl].x));
  double s, c;
  sincos(2.0 * CUDART_PI * ph, &s, &c);

  cuDoubleComplex res = make_cuDoubleComplex(c, -s); // to get rid of conj later

  mesh[xl][yl] = res;
  mesh[xl][yr] = res;
  mesh[xr][yl] = res;
  mesh[xr][yr] = res;
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
  __SET_MAP
  double ph = w * (1.0 - sqrt(1.0 - src[xl][yl].x));
  double s, c;
  sincos(2.0 * CUDART_PI * ph, &s, &c);

  cuDoubleComplex res = make_cuDoubleComplex(c, -s); // to get rid of conj later

  dst[xl][yl] = res;
  dst[xl][yr] = res;
  dst[xr][yl] = res;
  dst[xr][yr] = res;
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
    cuDoubleComplex dst[(max_half_support * 2 + 1) * oversample][(max_half_support * 2 + 1) * oversample]
  , const cuDoubleComplex src[max_half_support * 2 + 1][max_half_support * 2 + 1]
  ) {
  const int dst_center = (max_half_support * 2 + 1) * oversample / 2;
  __SET_MAP
  dst[dst_center - x][dst_center - y] = src[xl][yl];
  dst[dst_center - x][dst_center + y] = src[xl][yr];
  dst[dst_center + x][dst_center - y] = src[xr][yl];
  dst[dst_center + x][dst_center + y] = src[xr][yr];
}

// test instantiation
template __device__
void copy_ucs_2_over<256,8>(
    cuDoubleComplex dst[4104][4104]
  , const cuDoubleComplex src[513][513]
  );

#if 0
template <
    int max_half_support
  , int oversample
  >
__device__
void cut_out(
    int supp
  , cuDoubleComplex * dst
  , const cuDoubleComplex src[(max_half_support * 2 + 1) * oversample][(max_half_support * 2 + 1) * oversample]
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
  , const cuDoubleComplex src[4104][4104]
  );
#endif

template <
    int max_half_support
  , int oversample
  >
__device__
void extract_over(
    int overx
  , int overy
  , cuDoubleComplex dst[max_half_support * 2 + 1][max_half_support * 2 + 1]
  , const cuDoubleComplex src[(max_half_support * 2 + 1) * oversample][(max_half_support * 2 + 1) * oversample]
  ) {
  __SET_MAP
  const int
      sxl = xl * oversample + overx
    , sxr = xr * oversample + overx
    , syl = yl * oversample + overy
    , syr = yr * oversample + overy
    ;
  dst[xl][yl] = src[sxl][syl];
  dst[xl][yr] = src[sxl][syr];
  dst[xr][yl] = src[sxr][syl];
  dst[xr][yr] = src[sxr][syr];
  }

// test instantiation
template __device__
void extract_over<256,8>(
    int overx
  , int overy
  , cuDoubleComplex dst[513][513]
  , const cuDoubleComplex src[4104][4104]
  );

//
__device__ static __inline__ cuDoubleComplex cuMulComplexByDouble(cuDoubleComplex v,
                                                             double y)
{
  return make_cuDoubleComplex ( v.x * y
                              , v.y * y
                              );
}


// The work-distribution scheme here is very different from those above.
template <
    int max_half_support
  >
__device__
void normalize(
    double norm
  , cuDoubleComplex v[(max_half_support * 2 + 1) * (max_half_support * 2 + 1)]
  ) {
  const int x = blockIdx.x * blockDim.x + threadIdx.x;
  if (x < (max_half_support * 2 + 1) * (max_half_support * 2 + 1)) v[x] = cuMulComplexByDouble(v[x], norm);
}

// test instantiation
template __device__
void normalize<256>(
    double norm
  , cuDoubleComplex v[513*513]
  );
