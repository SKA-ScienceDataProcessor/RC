#include <cuComplex.h>

#include "common.h"
#include "metrix.h"

/* 128x128 bins, 8x8-blocks 16x16 threads each.
   BU - u-number of bin
   BV - v-number of bin
 */

template <
    bool is_half_gcf
  >
__device__ __inline__ static void grid_kernel_gather(
    int BU
  , int BV
  , const Pregridded * uvo
  , const Double4c * vis
  , int num_of_vals
  , const cuDoubleComplex * gcf[]
  , Double4c _grid[]
  , int grid_size
  , int bstep
  ){
  __ACC(Double4c, grid, grid_size);

  int gu = blockIdx.x * blockDim.x + threadIdx.x + BU * bstep;
  int gv = blockIdx.y * blockDim.y + threadIdx.y + BV * bstep;

  for (int i = 0; i < num_of_vals; i++){
    int du, dv, supp;
    du = gu - uvo[i].u;
    dv = gv - uvo[i].v;
    supp = uvo[i].gcf_layer_supp;
    // We have input u and v translated by -supp/2!
    if (du >= 0 && dv >= 0 && du < supp && dv < supp) {
      complexd supportPixel;
      #define __layeroff du * supp + dv
      if (is_half_gcf) {
        int index = uvo[i].gcf_layer_index;
        // Negative index indicates that original w was mirrored
        // and we shall negate the index to obtain correct
        // offset *and* conjugate the result.
        if (index < 0) {
          supportPixel = gcf[-index][__layeroff];
          supportPixel.y = - supportPixel.y;
        } else {
          supportPixel = gcf[index][__layeroff];
        }
      } else {
          supportPixel = gcf[uvo[i].gcf_layer_index][__layeroff];
      }
      #define __ADDPOL(pol) grid[gu][gv].pol = cuCfma(supportPixel, vis[i].pol, grid[gu][gv].pol);
      __ADDPOL(XX)
      __ADDPOL(XY)
      __ADDPOL(YX)
      __ADDPOL(XX)
    }
  }
}

#define gridKernelGather(suff, ishalf)             \
extern "C" __global__ void gridKernelGather##suff( \
    const Pregridded * uvo                         \
  , const Double4c * vis                           \
  , const cuDoubleComplex * gcf[]                  \
  , Double4c grid[]                                \
  , int BU                                         \
  , int BV                                         \
  , int num_of_vals                                \
  , int grid_size                                  \
  , int bstep                                      \
) {                                                \
  grid_kernel_gather<ishalf>(                      \
      BU                                           \
    , BV                                           \
    , uvo                                          \
    , vis                                          \
    , num_of_vals                                  \
    , gcf                                          \
    , grid                                         \
    , grid_size                                    \
    , bstep                                        \
    );                                             \
}

gridKernelGather(HalfGCF, true)
gridKernelGather(FullGCF, false)
