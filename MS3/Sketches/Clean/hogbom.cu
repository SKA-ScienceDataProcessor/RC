#include "threadFenceReduction_kernel.cuh"
#include "posFind.cuh"
#include <cstdlib>

#define __NBLOCKS(n,d) ((n)+(d)-1)/(d)

#define __CHECK if (err != cudaSuccess) return err;

inline
__host__ cudaError_t findPeak(
    const double * data_p
  , place * work_peak_p
  , place * peak_p
  , unsigned int n
  ) {
  cudaError_t err;

  err = resetRetirementCount();
  __CHECK
  findPeak_512_e2<<<__NBLOCKS(n, 1024), 512, 512 * sizeof(place)>>>(data_p, work_peak_p, n);
  err = cudaGetLastError();
  __CHECK
  err = cudaMemcpy(peak_p, work_peak_p, sizeof(place), cudaMemcpyDeviceToHost);
  __CHECK
  return cudaDeviceSynchronize();
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

typedef long long int lli;

inline
__host__
static void subtractPSF(
          double * res_p
  , const double * psf_p
  , const lli peak_res_pos
  , const lli peak_psf_pos
  , const int linsize
  , const double peak_x_gain
  ) {
  const lli diff = peak_psf_pos - peak_res_pos;
  lldiv_t
      resxy = div(peak_res_pos, (lli)linsize)
    , psfxy = div(peak_psf_pos, (lli)linsize)
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
    subtract_psf_kernel<<<numBlocks, threadsPerBlock>>>(res_p, psf_p + diff, stopx, stopy, diff, linsize, peak_x_gain);
  else
    subtract_psf_kernel<<<numBlocks, threadsPerBlock>>>(res_p - diff, psf_p, stopx, stopy, diff, linsize, peak_x_gain);
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
  place * peakp;
  place
      found_place_psf
    , found_place_res
    ;
  const int totsize = linsize * linsize;

  // working space to find peaks
  err = cudaMalloc((void **) &peakp, __NBLOCKS(totsize, 1024) * sizeof(place));
  __CHECK

#undef __CHECK
#define __CHECK if (err != cudaSuccess) goto fin;

  err = findPeak(psf_p, peakp, &found_place_psf, totsize);
  __CHECK

  for (unsigned int i = 0; i < niters; ++i) {
    err = findPeak(res_p, peakp, &found_place_res, totsize);
    __CHECK

    if (abs(found_place_res.val) < threshold) break;

    subtractPSF(res_p, psf_p, found_place_res.pos, found_place_psf.pos, linsize, found_place_res.val * gain);
    mod_p[found_place_res.pos] += found_place_res.val * gain;
    // Wait for the PSF subtraction to finish
    err = cudaDeviceSynchronize();
  }

  fin:;
  cudaFree(peakp);
  return err;
}
