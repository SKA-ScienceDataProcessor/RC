#include <cuda.h>
#include <cuComplex.h>

// Launch configuration should be as follows:
//  1. blocks of dim3(threads_per_dim, threads_per_dim, 1) size
//       where threads_per_dim = min(N, 16)
//  2. grid of dim3((N+threads_per_dim-1)/threads_per_dim, (N-1)/(threads_per_dim * 2)+1, 1) blocks

template <class T>
static __device__ __inline__
void fftshift_kernel_common(T* data, int shift, int N, int pitch){
  int x = blockIdx.x * blockDim.x + threadIdx.x;
  int y = blockIdx.y * blockDim.y + threadIdx.y;

  if (x > N-1 || y > (N+1)/2-1) return;

  int x1 = x + shift;
  if (x1 > N-1) x1 -= N;

  int y1 = y + shift;
  if (y1 > N-1) y1 -= N;

  int
      i = x + y * pitch
    , i1 = x1 + y1 * pitch;

  T tmp;
  tmp = data[i];
  data[i] = data[i1];
  data[i1] = tmp;
}

extern "C" {
__global__
void fftshift_half_hermitian(cuDoubleComplex* data, int N, int pitch) {
  int x = blockIdx.x * blockDim.x + threadIdx.x;
  int y = blockIdx.y * blockDim.y + threadIdx.y;
  if (x >= N/4 || y >= N) return;
  // shift+mirror
  int x1 = N/2 - x - 1; // N - (N + N/2) - 1
  int y1 = N/2 - y - 1; // N - (N + N/2) - 1
  if (y1 < 0) y1 += N;
  // offsets
  int i = x+y*pitch,
      i1 = x1+y1*pitch;
  // Swap + conjugate (note that x == N/2-x1-1!)
  cuDoubleComplex tmp;
  tmp = data[i];
  data[i].x = data[i1].x;
  data[i].y = -data[i1].y;
  data[i1].x = tmp.x;
  data[i1].y = -tmp.y;
}

__global__
void fftshift_kernel_cx(cuDoubleComplex* data, int N, int pitch) {
  fftshift_kernel_common<cuDoubleComplex>(data, N/2, N, pitch);
}

__global__
void ifftshift_kernel_cx(cuDoubleComplex* data, int N, int pitch) {
  fftshift_kernel_common<cuDoubleComplex>(data, N/2 + N%2, N, pitch);
}

__global__
void fftshift_kernel_r(double* data, int N, int pitch) {
  fftshift_kernel_common<double>(data, N/2, N, pitch);
}

__global__
void ifftshift_kernel_r(double* data, int N, int pitch) {
  fftshift_kernel_common<double>(data, N/2 + N%2, N, pitch);
}
}
