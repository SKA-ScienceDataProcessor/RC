#include "GCF.cu"
#include "../FFT/cufftshift.cu"

#include "threadFenceReduction_kernel.cuh"

__device__ double norm;

extern "C" __global__ void reduce_512_odd(const cuDoubleComplex *g_idata, unsigned int n) {
  reduceSinglePass_dev<512, false>(g_idata, &norm, n);
}

extern "C" __global__ void normalize(
    cuDoubleComplex v[257*257]
  ){
  normalize_kernel<128>(norm, v);
}
