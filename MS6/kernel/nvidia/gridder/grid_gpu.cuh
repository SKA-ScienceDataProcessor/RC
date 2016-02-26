#ifndef __GRID_CUH
#define __GRID_CUH

#if defined __CUDACC__ || defined __CUDA__
#include <cuda.h>
typedef double2 CmplxType;
#else
struct CmplxType;
struct double3 {double x, y, z;};
#endif

struct combined {
  double x, y, z, a_re, a_im;
};

// GCF size is hardcoded and defined in Defines.h

#ifdef __MK_DLL
#define EXPORT __declspec(dllexport)
#else
#define EXPORT 
#endif

extern "C" {

EXPORT
void bin_sort3(double3 * in, int npts);
EXPORT
void bin_sort5(combined * in, int npts);
EXPORT
void gridGPUs( double scale
             , CmplxType* out
             , double3 * in, CmplxType* in_vals
             , size_t npts, size_t img_dim, CmplxType *gcf
             );
EXPORT
void gridGPUc( double scale
             , CmplxType* out
             , combined * in_c
             , size_t npts, size_t img_dim, CmplxType *gcf
             );

}

#endif
