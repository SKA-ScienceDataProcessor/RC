#ifndef __GRID_CUH
#define __GRID_CUH

#ifndef __clang__
#include <cuda.h>
#else
#include "C:\Progs\clang-HEAD-x86_64-w64-windows-gnu-s\lib\clang\3.9.0\include\__clang_cuda_runtime_wrapper.h"
#endif

typedef double2 CmplxType;

struct combined {
  double x, y, z, a_re, a_im;
};

// GCF size is hardcoded and defined in Defines.h

extern "C" {

void bin_sort3(double3 * in, int npts);
void bin_sort5(combined * in, int npts);
void gridGPUs( double scale
             , CmplxType* out
             , double3 * in, CmplxType* in_vals
             , size_t npts, size_t img_dim, CmplxType *gcf
             );
void gridGPUc( double scale
             , CmplxType* out
             , combined * in_c
             , size_t npts, size_t img_dim, CmplxType *gcf
             );

}

#endif
