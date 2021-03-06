#ifndef __COMMON_H
#define __COMMON_H

#if defined __AVX__
#include <immintrin.h>
#define __MMSIZE 32
typedef __m256d __mdType;
#define asMdp(p) (reinterpret_cast<__m256d*>(p))
#define asMdpc(p) (reinterpret_cast<const __m256d*>(p))
#define _mm_add_pd _mm256_add_pd
#else
#include <smmintrin.h>
#define __MMSIZE 16
typedef __m128d __mdType;
#define asMdp(p) (reinterpret_cast<__m128d*>(p))
#define asMdpc(p) (reinterpret_cast<const __m128d*>(p))
#endif

#ifdef __CUDACC__
  #include <cuComplex.h>
  typedef cuDoubleComplex complexd;
  #include <math_constants.h>
  #define __PI CUDART_PI
  #define __HOST __host__
  #define __NATIVE0 __device__ __inline__
  #define __NATIVE __device__ __inline__ static
#else
  #ifdef _MSC_VER
  #define sincos(v,sp,cp) *(sp)=sin(v);*(cp)=cos(v)
  #endif
  #define cuCmul(a,b) ((a)*(b))
  #define _USE_MATH_DEFINES
  #include <math.h>
  #define __PI M_PI
  #ifdef __cplusplus
    #include <complex>
    typedef std::complex<double> complexd;
    #define make_cuDoubleComplex(r,i) (complexd(r,i))
  #else
    #include <complex.h>
    typedef double complex complexd;
    #define make_cuDoubleComplex(r,i) ((r)+I*(i))
  #endif
  #define __HOST
  #define __NATIVE0 __inline
  #define __NATIVE __inline static
#endif

// Tiny 2d accessor class to make code look
//   the same when accessing statically and dynamically sized 2d arrays
template <typename t> struct acc2d {
  __NATIVE0 acc2d(t * ptr, int p) : dptr(ptr), pitch(p) {;}
  __NATIVE0 acc2d(const t * ptr, int p) : pitch(p) {dptr = const_cast<t*>(ptr);}
  __NATIVE0 t * operator[](int i){return dptr + i*pitch;}

private:
  t * dptr;
  int pitch;
};

#define __PASTE(a,b) a##b
#define __ACC(typ, id, pitch)  acc2d<typ> id(__PASTE(_,id), pitch);

struct Double4c
{
  complexd XX;
  complexd XY;
  complexd YX;
  complexd YY;
};

struct Double3
{
  double u;
  double v;
  double w;
};

struct Pregridded
{
  short u;
  short v;
  short gcf_layer_index;
  union {
    short w_plane;
    short gcf_layer_supp;
  };
};

__NATIVE complexd rotw(complexd v, double w){
  double s, c;
  sincos(2.0 * __PI * w, &s, &c);
  return cuCmul(v, make_cuDoubleComplex(c, s));
}

#if defined __cplusplus || defined __CUDACC__
template <
#ifndef __DYN_GRID_SIZE
    int grid_size,
#endif
    int over
  , bool do_mirror
  >
#ifndef __CUDACC__
inline
#else
// FIXME: this is a bit messy because of renaming.
// Investigate how to get rid of this better.
#define Double3 double3
#define u x
#define v y
#define w z
__inline__ __device__
#endif
static void pregridPoint(double scale, double wstep, Double3 uvw
#ifndef __BINNER
    , int max_supp
#endif
    , Pregridded & res
#if defined __DYN_GRID_SIZE && !defined __BINNER
    , int grid_size
#endif
  ){
    uvw.u *= scale;
    uvw.v *= scale;
    // Since we use unscaled wstep, we shouldn't scale uvw.w either
    // uvw.w *= scale;
    short
        w_plane = short(round (uvw.w / wstep))
      , u = short(floor(uvw.u))
      , v = short(floor(uvw.v))
      , over_u = short(floor(over * (uvw.u - u)))
      , over_v = short(floor(over * (uvw.v - v)))
      ;
    // We additionally translate these u v by -max_supp/2
    // because gridding procedure translates them back
    // Binner makes this separately.
#ifndef __BINNER
    u += short(grid_size / 2 - max_supp / 2);
    v += short(grid_size / 2 - max_supp / 2);
#endif
#ifndef __CUDACC__
    res.u = u;
    res.v = v;
#else
#undef u
#undef v
#undef w
    res.u = x;
    res.v = y;
#endif
    // Well, this is a kind of trick:
    // For full GCF we can have w_plane and hence gcf_layer_index negative.
    // But for half GCF we have both positive. Thus to convey an information
    // about original w being negative we negate the whole index.
    // When inspecting it client of half GCF if looking negative index should
    // both negate it *and* conjugate support pixel.
    if (do_mirror) {
      if (w_plane < 0) {
          res.gcf_layer_index = -((-w_plane * over + over_u) * over + over_v);
      } else {
          res.gcf_layer_index = (w_plane * over + over_u) * over + over_v;
      }
    } else {
      res.gcf_layer_index = (w_plane * over + over_u) * over + over_v;
    }
    // Binner replaces
    // w_plane with gcf_layer_supp
    res.w_plane = w_plane;
}
#endif

// We have original u,v,w, in meters.
// To go to u,v,w in wavelengths we shall multiply them with freq/SPEED_OF_LIGHT

#ifndef SPEED_OF_LIGHT
#define SPEED_OF_LIGHT 299792458.0
#endif

#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
#define __OLD
#endif

#ifdef __OLD
#define nullptr NULL
#endif

#endif
