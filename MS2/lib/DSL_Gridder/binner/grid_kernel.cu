#include <cuComplex.h>

#include "metrix.h"

typedef cuDoubleComplex grid_t[GRID_D][GRID_D][NUM_POL];
typedef cuDoubleComplex gcf_t[WPLANES][OVER][OVER][SUPP_D][SUPP_D];

/* 128x128 bins, 8x8-blocks 16x16 threads each.
   BU - u-number of bin
   BV - v-number of bin
 */

extern "C"
__global__ void grid_kernel(int BU, int BV, vis_data * vals, int num_of_vals, const gcf_t * gcfp, grid_t * gridp){
  int gu = blockIdx.x * blockDim.x + threadIdx.x + BU * BSTEP;
  int gv = blockIdx.y * blockDim.y + threadIdx.y + BV * BSTEP;

  for (int i = 0; i < num_of_vals; i++){
    int du = vals[i].u - gu;
    int dv = vals[i].v - gv;
    if (abs(du) < SUPP_H && abs(dv) < SUPP_H) {
      int su = SUPP_H + du;
      int sv = SUPP_H + dv;
#define gcf (*gcfp)
#define grid (*gridp)
      grid[gu][gv][0] = cuCfma(vals[i].XX, gcf[vals[i].wplane][vals[i].fracu][vals[i].fracv][su][sv], grid[gu][gv][0]);
      grid[gu][gv][1] = cuCfma(vals[i].XY, gcf[vals[i].wplane][vals[i].fracu][vals[i].fracv][su][sv], grid[gu][gv][1]);
      grid[gu][gv][2] = cuCfma(vals[i].YX, gcf[vals[i].wplane][vals[i].fracu][vals[i].fracv][su][sv], grid[gu][gv][2]);
      grid[gu][gv][3] = cuCfma(vals[i].YY, gcf[vals[i].wplane][vals[i].fracu][vals[i].fracv][su][sv], grid[gu][gv][3]);
    }
  }
}
