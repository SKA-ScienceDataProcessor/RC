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
__device__ __inline__
void ucs_common(
    cuDoubleComplex mesh[max_half_support * 2 + 1][max_half_support * 2 + 1]
  , double t2
  ){
  __SET_MAP
  double
      t2_div_sc = t2 / double(max_half_support)
    , xs = double(x) * t2_div_sc
    , ys = double(y) * t2_div_sc
    ;
  cuDoubleComplex r2 = make_cuDoubleComplex(xs * xs + ys * ys, 0.0);

  mesh[xl][yl] = r2;
  mesh[xl][yr] = r2;
  mesh[xr][yl] = r2;
  mesh[xr][yr] = r2;
}

extern "C" __global__ void r2(cuDoubleComplex mesh[513][513], double t2) {
  ucs_common<256>(mesh, t2);
}


#if 0
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
#endif


template <int max_half_support>
__device__ __inline__
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

extern "C" __global__ void wkernff(
    cuDoubleComplex dst[513][513]
  , const cuDoubleComplex src[513][513]
  , double w
  ){
  calc<256>(dst, src, w);
}


template <
    int max_half_support
  , int oversample
  >
__device__ __inline__
void copy_ucs_2_over(
    cuDoubleComplex dst[(max_half_support * 2 + 1) * oversample][(max_half_support * 2 + 1) * oversample]
  , const cuDoubleComplex src[max_half_support * 2 + 1][max_half_support * 2 + 1]
  ){
  const int dst_center = (max_half_support * 2 + 1) * oversample / 2;
  __SET_MAP
  dst[dst_center - x][dst_center - y] = src[xl][yl];
  dst[dst_center - x][dst_center + y] = src[xl][yr];
  dst[dst_center + x][dst_center - y] = src[xr][yl];
  dst[dst_center + x][dst_center + y] = src[xr][yr];
}

extern "C" __global__  void copy_2_over(
    cuDoubleComplex dst[4104][4104]
  , const cuDoubleComplex src[513][513]
  ){
  copy_ucs_2_over<256,8>(dst, src);
}


template <
    int max_half_support
  , int oversample
  >
__device__ __inline__
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


extern "C" __global__ void wextract0(
    int overx
  , int overy
  , cuDoubleComplex dst[513][513]
  , const cuDoubleComplex src[4104][4104]
  ){
  extract_over<256,8>(overx, overy, dst, src);
}


//
__device__ static __inline__ cuDoubleComplex cuMulComplexByDouble(cuDoubleComplex v,
                                                             double y){
  return make_cuDoubleComplex ( v.x * y
                              , v.y * y
                              );
}

// The work-distribution scheme here is very different from those above (and below).
template <
    int max_half_support
  >
__device__ __inline__
void normalize_kernel(
    double norm
  , cuDoubleComplex v[(max_half_support * 2 + 1) * (max_half_support * 2 + 1)]
  ) {
  const int x = blockIdx.x * blockDim.x + threadIdx.x;
  if (x < (max_half_support * 2 + 1) * (max_half_support * 2 + 1)) v[x] = cuMulComplexByDouble(v[x], norm);
}

extern "C" __global__ void normalize(
    double norm
  , cuDoubleComplex v[513*513]
  ){
  normalize_kernel<256>(norm, v);
}


template <
    int max_half_support
  , int oversample
  >
__device__ __inline__
void cut_out(
    int half_supp
  , cuDoubleComplex * dst
  , const cuDoubleComplex src[max_half_support * 2 + 1][max_half_support * 2 + 1]
  ) {
  __SET_MAP
  const int supp = half_supp * 2 + 1;

  if (x > half_supp || y > half_supp) return;

  const int
      dxl = (half_supp - x) * supp
    , dxr = (half_supp + x) * supp
    , dyl = half_supp - y
    , dyr = half_supp + y
    ;

  dst[dxl + dyl] = src[xl][yl];
  dst[dxl + dyr] = src[xl][yr];
  dst[dxr + dyl] = src[xr][yl];
  dst[dxr + dyr] = src[xr][yr];
}

extern "C" __global__ void wextract1(
    int supp
  , cuDoubleComplex * dst
  , const cuDoubleComplex src[513][513]
  ){
  cut_out<256,8>(supp, dst, src);
}
