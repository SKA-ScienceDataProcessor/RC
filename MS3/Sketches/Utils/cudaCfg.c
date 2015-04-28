#include <cuda_runtime_api.h>

cudaError_t
__cudaConfigureCall
(
    int gridX,  int gridY, int gridZ,
    int blockX, int blockY, int blockZ,
    size_t sharedMem,
    cudaStream_t stream
)
{
    dim3 gridDim  = {gridX, gridY, gridZ};
    dim3 blockDim = {blockX,blockY,blockZ};

    return cudaConfigureCall(gridDim, blockDim, sharedMem, stream);
}
