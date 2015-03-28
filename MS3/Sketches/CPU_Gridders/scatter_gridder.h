#ifndef __SCATTER_GRIDDER_H
#define __SCATTER_GRIDDER_H

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

// We have original u,v,w, in meters.
// To go to u,v,w in wavelengths we shall multiply them with freq/SPEED_OF_LIGHT

#ifndef SPEED_OF_LIGHT
#define SPEED_OF_LIGHT 299792458.0
#endif

#ifndef WSTEP_CORRECT
#define WSTEP_CORRECT 0.00001
#endif

struct TaskCfg {
  double
      min_wave_length
    , max_inverse_wave_length
    , cellsize
    , cellsizeWL
    , scale
    , scaleWL
    , w_step
    , w_stepWL
    , w_shift
    , w_shiftWL
    ;
};

#ifdef __CUDACC__
__device__ __inline__ static 
#else
__inline static
#endif
  int get_supp(int w) {
    if (w < 0) w = -w;
    return w * 8 + 1;
  }

#endif
