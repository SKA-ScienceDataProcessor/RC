#include "scatter_gridder.h"

__device__ double atomicAdd(double* address, double val)
{
#ifdef FAKE_ATOMIC
  double orig = *address;
  *address = orig + val;
  return orig;
#else
  unsigned long long int* address_as_ull = (unsigned long long int*)address;
  unsigned long long int old = *address_as_ull, assumed;
  do {
       assumed = old;
       old = atomicCAS(address_as_ull, assumed,
                         __double_as_longlong(val +
                           __longlong_as_double(assumed)));
       // Note: uses integer comparison to avoid hang in case of NaN (since NaN != NaN)
  } while (assumed != old);
  return __longlong_as_double(old);
#endif
}

__device__ __inline__ void atomicAdd(Double4c *ptr, complexd sumXX, complexd sumXY, complexd sumYX, complexd sumYY)
{
  atomicAdd(&ptr->XX.x, sumXX.x);
  atomicAdd(&ptr->XX.y, sumXX.y);
  atomicAdd(&ptr->XY.x, sumXY.x);
  atomicAdd(&ptr->XY.y, sumXY.y);
  atomicAdd(&ptr->YX.x, sumYX.x);
  atomicAdd(&ptr->YX.y, sumYX.y);
  atomicAdd(&ptr->YY.x, sumYY.x);
  atomicAdd(&ptr->YY.y, sumYY.y);
}

struct Pregridded
{
  short u;
  short v;
  int gcf_layer_offset;
};

template <
    int w_planes
  , int max_supp
  , int grid_size
  , int over

  , int baselines
  , int timesteps
  , int channels
  >
__global__
// grid must be initialized to 0s.
void gridKernel_scatter(
    const complexd gcf[w_planes][over][over][max_supp][max_supp]
  , Double4c grid[grid_size][grid_size]
  , const Pregridded uvw[baselines][timesteps * channels]
  , const Double4c vis[baselines][timesteps * channels]
  ) {
  // We use 2-dimensional threadblocks (max_supp x max_supp)
  // but the idea is entirely the same as Romein's
  // FIXME: We are limited to 32x32 gcf layers thus. Add extra grid or block dimension
  //   to account for this!!!
  int
      myU = threadIdx.x
    , myV = threadIdx.y
    , bl = blockIdx.x
    ;
  complexd
      sumXX = {0, 0}
    , sumXY = {0, 0}
    , sumYX = {0, 0}
    , sumYY = {0, 0}
    ;
  int
      grid_point_u = 0
    , grid_point_v = 0;

  for (int i = 0; i < timesteps * channels; i++) {
    int myConvU, myConvV, myGridU, myGridV, u, v;
    u = uvw[bl][i].u;
    v = uvw[bl][i].v;
    myConvU = (u + max_supp/2) % max_supp - myU;
    myConvV = (v + max_supp/2) % max_supp - myV;
    myGridU = u - myConvU;
    myGridV = v - myConvV;
    complexd supportPixel = (gcf[0][0][0] + uvw[bl][i].gcf_layer_offset)[myConvU][myConvV];
    if (myGridU != grid_point_u || myGridV != grid_point_v) {
      atomicAdd(&grid[grid_point_u][grid_point_v], sumXX, sumXY, sumYX, sumYY);
        sumXX
      = sumXY
      = sumYX
      = sumYY
      = {0, 0};
      grid_point_u = myGridU;
      grid_point_v = myGridV;
    }
    #define __ADD_SUPP(pol) sum##pol = cuCfma(supportPixel, vis[bl][i].pol, sum##pol)
    __ADD_SUPP(XX);
    __ADD_SUPP(XY);
    __ADD_SUPP(YX);
    __ADD_SUPP(YY);
  }
  atomicAdd(&grid[grid_point_u][grid_point_v], sumXX, sumXY, sumYX, sumYY);
}

// Test instantiation
template __global__ void gridKernel_scatter<32, 64, 2048, 8, 50*99, 20, 1>(
    const complexd gcf[32][8][8][64][64]
  , Double4c grid[2048][2048]
  , const Pregridded uvw[(50*90)][20 * 1]
  , const Double4c vis[(50*90)][20 * 1]
  );
