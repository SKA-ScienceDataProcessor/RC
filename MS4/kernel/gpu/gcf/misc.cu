
#include "threadFenceReduction_kernel.cuh"

struct doubleSum {
  TASKCFG double init() {return 0.0;}
  TASKCFG double reduce(double x, double acc){return x + acc;}
  TASKCFG double f(unsigned int, cuDoubleComplex c){return c.x;}
};

template <unsigned int blockSize, bool nIsPow2>
TASKCFG
void reduceSinglePass_dev(const cuDoubleComplex *g_idata, double *g_odata, unsigned int n){
  reduceSinglePass_devGen<blockSize, nIsPow2, double, cuDoubleComplex, doubleSum>(g_idata, g_odata, n);
}

extern "C" __host__
void reduce_init() {
    resetRetirementCount();
}

extern "C" __global__ void reduce_512_e2(const cuDoubleComplex *g_idata, double *g_odata, unsigned int n) {
  reduceSinglePass_dev<512, true>(g_idata, g_odata, n);
}

__device__ static __inline__ cuDoubleComplex cuMulComplexByDouble(cuDoubleComplex v,
                                                             double y){
  return make_cuDoubleComplex ( v.x * y
                              , v.y * y
                              );
}

__device__ __inline__
void scale_complex_by_dbl(
    double norm
  , cuDoubleComplex * v
  , int len
  ) {
  const int x = blockIdx.x * blockDim.x + threadIdx.x;
  if (x < len) v[x] = cuMulComplexByDouble(v[x], norm);
}

extern "C" __global__ void normalize(
    double * normp
  , cuDoubleComplex * v
  , int len
  ){
  scale_complex_by_dbl(1.0 / (*normp), v, len);
}

// Helper for calling cudaConfigureCall from Haskell-land
extern "C" cudaError_t __cudaConfigureCall
(
    int gridX,  int gridY, int gridZ,
    int blockX, int blockY, int blockZ,
    size_t sharedMem,
    cudaStream_t stream
)
{
    dim3 gridDim(gridX, gridY, gridZ);
    dim3 blockDim(blockX,blockY,blockZ);

    return cudaConfigureCall(gridDim, blockDim, sharedMem, stream);
}
