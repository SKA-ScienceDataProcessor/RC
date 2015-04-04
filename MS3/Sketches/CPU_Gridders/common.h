#ifndef __COMMON_H
#define __COMMON_H

#if defined __AVX__
#include <immintrin.h>
#endif

#ifdef __CUDACC__
  #include <cuComplex.h>
  typedef cuDoubleComplex complexd;
#elif defined __cplusplus
  #include <complex>
  typedef std::complex<double> complexd;
#else
  #include <complex.h>
  typedef double complex complexd;
#endif

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
  short gcf_layer_supp;
};

// We have original u,v,w, in meters.
// To go to u,v,w in wavelengths we shall multiply them with freq/SPEED_OF_LIGHT

#ifndef SPEED_OF_LIGHT
#define SPEED_OF_LIGHT 299792458.0
#endif

#ifdef __CUDACC__
__device__ __inline__ static 
#else
__inline static
#endif
  int get_supp(int w) {
    if (w < 0) w = -w;
    return w * 8 + 1;
  }

#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
#define __OLD
#endif

#ifdef __OLD
#define nullptr NULL
#endif

#endif
