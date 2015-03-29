#include "scale_complex_by_dbl.cuh"
#include "GCF.cu"
#include "../FFT/cufftshift.cu"

#include "threadFenceReduction_kernel.cuh"

extern "C" __global__ void reduce_512_odd(const cuDoubleComplex *g_idata, double *g_odata, unsigned int n) {
  reduceSinglePass_dev<512, false>(g_idata, g_odata, n);
}

extern "C" __global__ void normalize(
    double * normp
  , cuDoubleComplex * v
  , int len
  ){
  scale_complex_by_dbl(1.0 / (*normp), v, len);
}
