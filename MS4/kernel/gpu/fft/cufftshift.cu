#include <cuda.h>
#include <cuComplex.h>

// Launch configuration should be as follows:
//  1. blocks of dim3(threads_per_dim, threads_per_dim, 1) size
//       where threads_per_dim = min(N, 16)
//  2. grid of dim3((N+threads_per_dim-1)/threads_per_dim, (N-1)/(threads_per_dim * 2)+1, 1) blocks

static __device__ __inline__
void fftshift_kernel_common(cuDoubleComplex* data, int shift, int N){
  int x = blockIdx.x * blockDim.x + threadIdx.x;
  int y = blockIdx.y * blockDim.y + threadIdx.y;

  if (x > N-1 || y > (N+1)/2-1) return;

  int x1 = x + shift;
  if (x1 > N-1) x1 -= N;

  int y1 = y + shift;
  if (y1 > N-1) y1 -= N;

  int
      i = x + y * N
    , i1 = x1 + y1 * N;

  cuDoubleComplex tmp;
  tmp = data[i];
  data[i] = data[i1];
  data[i1] = tmp;
}

extern "C" {
__global__
void fftshift_kernel(cuDoubleComplex* data, int N) {
  fftshift_kernel_common(data, N/2, N);
}

__global__
void ifftshift_kernel(cuDoubleComplex* data, int N) {
  fftshift_kernel_common(data, N/2 + N%2, N);
}
}
