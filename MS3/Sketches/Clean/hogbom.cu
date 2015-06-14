#include "threadFenceReduction_kernel.cuh"
#include "posFind.cuh"
#include <cstdlib>

static place found_place_res;
static place found_place_psf;

#define __NBLOCKS(n,d) ((n)+(d)-1)/(d)

inline
__host__ cudaError_t findPeaks(
    const double * res_p
  , const double * psf_p
  , unsigned int n
  ) {
  cudaError_t err;
  place * peakp;
  unsigned int nbl = __NBLOCKS(n, 1024);
  int shmemsiz = 512 * sizeof(place);

  err = cudaMalloc((void **) &peakp, nbl * sizeof(place));
  if (err != cudaSuccess) return err;

#define __CHECK if (err != cudaSuccess) goto fin;
  err = resetRetirementCount();
  __CHECK
  findPeak_512_e2<<<nbl, 512, shmemsiz>>>(res_p, peakp, n);
  err = cudaGetLastError();
  __CHECK
  err = cudaMemcpy(&found_place_res, peakp, sizeof(place), cudaMemcpyDeviceToHost);
  __CHECK
  err = cudaDeviceSynchronize();
  __CHECK

  err = resetRetirementCount();
  __CHECK
  findPeak_512_e2<<<nbl, 512, shmemsiz>>>(psf_p, peakp, n);
  err = cudaGetLastError();
  __CHECK
  err = cudaMemcpy(&found_place_psf, peakp, sizeof(place), cudaMemcpyDeviceToHost);
  __CHECK
  err = cudaDeviceSynchronize();

  fin:;
  cudaFree(peakp);
  return err;
}

static
__global__ void subtract_psf_kernel(
          double * res_p_trans
  , const double * psf_p_trans
  , const int stopx
  , const int stopy
  , const int diff
  , const int linsize
  , const double peak_x_gain
  ) {
    const int
        x =  threadIdx.x + (blockIdx.x * blockDim.x)
      , y =  threadIdx.y + (blockIdx.y * blockDim.y)
      , tid = y * linsize + x
      ;
    if (x < stopx && y < stopx) res_p_trans[tid] -= peak_x_gain * psf_p_trans[tid + diff];
}

#define __BLOCK_DIM 16

inline
__host__
static void subtractPSF(
          double * res_p
  , const double * psf_p
  , const int linsize
  , const double gain
  ) {
  typedef long long int lli;
  const lli diff = (lli)found_place_psf.pos - (lli)found_place_res.pos;
  lldiv_t
      resxy = div(found_place_res.pos, (lli)linsize)
    , psfxy = div(found_place_psf.pos, (lli)linsize)
    ;
  const int
      stopx = linsize - abs (psfxy.rem - resxy.rem)
    , stopy = linsize - abs (psfxy.quot - resxy.quot)
    , blocksx = __NBLOCKS(stopx, __BLOCK_DIM)
    , blocksy = __NBLOCKS(stopy, __BLOCK_DIM)
    ;

  dim3 numBlocks(blocksx, blocksy);
  dim3 threadsPerBlock(__BLOCK_DIM, __BLOCK_DIM);

  if (diff >= 0)
    subtract_psf_kernel<<<numBlocks, threadsPerBlock>>>(res_p, psf_p + diff, stopx, stopy, diff, linsize, found_place_res.val * gain);
  else
    subtract_psf_kernel<<<numBlocks, threadsPerBlock>>>(res_p - diff, psf_p, stopx, stopy, diff, linsize, found_place_res.val * gain);
}

// All pointers are device pointers
__host__
cudaError_t deconvolve(
          double * mod_p
  ,       double * res_p
  , const double * psf_p
  , const int linsize
  , const unsigned int niters
  , const double gain
  , const double threshold
  ) {
  cudaError_t err;

  for (unsigned int i = 0; i < niters; ++i) {
    err = findPeaks(res_p, psf_p, linsize * linsize);
    if (err != cudaSuccess) return err;

    if (abs(found_place_res.val) < threshold) break;

    subtractPSF(res_p, psf_p, linsize, gain);
    mod_p[found_place_res.pos] += found_place_res.val * gain;
    // Wait for the PSF subtraction to finish
    err = cudaDeviceSynchronize();
  }

  return err;
}
