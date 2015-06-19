#include "common.h"
#include "metrix.h"
#include "atomic_add.h"
#include "OskarBinReader.h"

#include <math_functions.h>

template <
    int timesteps
  , int channels
  >
__inline__ __device__ void loadVisIntoSharedMem (
    const Double4c vis[timesteps * channels]
  , Double4c vis_shared[timesteps * channels]
  ) {
  for (int i = threadIdx.x; i < timesteps * channels; i += blockDim.x) {
    vis_shared[i] = vis[i];
  }
}

template <
    int over
  , int grid_size

  , int timesteps
  , int channels
  , bool do_mirror
  >
__inline__ __device__ void loadUVWIntoSharedMem (
    double scale
  , double wstep
  , int max_supp // needed for u v translation
  , const double3 uvw[timesteps * channels]
  , Pregridded uvo_shared[timesteps * channels]
  , int2 off_shared[timesteps * channels]
  ) {
  for (int i = threadIdx.x; i < timesteps * channels; i += blockDim.x) {
    // uvo_shared[i] is passed by reference and updated!
    pregridPoint<grid_size, over, do_mirror>(scale, wstep, uvw[i], uvo_shared[i]);
    off_shared[i].x = uvo_shared[i].u % max_supp;
    off_shared[i].y = uvo_shared[i].v % max_supp;
  }
}

template <
    int grid_size

  , int timesteps
  , int channels
  , bool is_half_gcf
  >
__inline__ __device__
// grid must be initialized to 0s.
void gridKernel_scatter_kernel_small(
    int max_supp
    // For full-size GCF should be passed 0-centered,
    // i.e. with 0-index in the middle
  , const complexd * gcf[]
  , Double4c grid[grid_size][grid_size]
  , const Pregridded uvo_shared[timesteps * channels]
  , const Double4c vis[timesteps * channels]
  , const int2 off_shared[timesteps * channels]
  , int myU
  , int myV
  ) {
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
    u = uvo_shared[i].u;
    v = uvo_shared[i].v;
    myConvU = myU - off_shared[i].x;
    if (myConvU < 0) myConvU += max_supp;
    myConvV = myV - off_shared[i].y;
    if (myConvV < 0) myConvV += max_supp;
    // This translates points by max_supp/2
    // returning them back to normal (they were translates by -max_supp/2 before)
    myGridU = u + myConvU;
    myGridV = v + myConvV;

    int supp = uvo_shared[i].gcf_layer_supp;

    complexd supportPixel;
    #define __layeroff myConvU * supp + myConvV
    if (is_half_gcf) {
      int index = uvo_shared[i].gcf_layer_index;
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
        supportPixel = gcf[uvo_shared[i].gcf_layer_index][__layeroff];
    }

    if (myGridU != grid_point_u || myGridV != grid_point_v) {
      atomicAdd(&grid[grid_point_u][grid_point_v], sumXX, sumXY, sumYX, sumYY);
        sumXX
      = sumXY
      = sumYX
      = sumYY
      = make_cuDoubleComplex(0.0, 0.0);
      grid_point_u = myGridU;
      grid_point_v = myGridV;
    }
    #define __ADD_SUPP(pol) sum##pol = cuCfma(supportPixel, vis[i].pol, sum##pol)
    __ADD_SUPP(XX);
    __ADD_SUPP(XY);
    __ADD_SUPP(YX);
    __ADD_SUPP(YY);
  }
  atomicAdd(&grid[grid_point_u][grid_point_v], sumXX, sumXY, sumYX, sumYY);
}

////////////////////////////////////
template <
    int grid_size

  , int timesteps
  , int channels
  , bool is_half_gcf
  >
__inline__ __device__
// psfi must be initialized to 0s.
void psfiKernel_scatter_kernel_small(
    int max_supp
    // For full-size GCF should be passed 0-centered,
    // i.e. with 0-index in the middle
  , const complexd * gcf[]
  , complexd psfi[grid_size][grid_size]
  , const Pregridded uvo_shared[timesteps * channels]
  , const int2 off_shared[timesteps * channels]
  , int myU
  , int myV
  ) {
  complexd sum = {0, 0};
  int
      grid_point_u = 0
    , grid_point_v = 0;

  for (int i = 0; i < timesteps * channels; i++) {
    int myConvU, myConvV, myGridU, myGridV, u, v;
    u = uvo_shared[i].u;
    v = uvo_shared[i].v;
    myConvU = myU - off_shared[i].x;
    if (myConvU < 0) myConvU += max_supp;
    myConvV = myV - off_shared[i].y;
    if (myConvV < 0) myConvV += max_supp;
    // This translates points by max_supp/2
    // returning them back to normal (they were translates by -max_supp/2 before)
    myGridU = u + myConvU;
    myGridV = v + myConvV;

    int supp = uvo_shared[i].gcf_layer_supp;

    complexd supportPixel;
    #define __layeroff myConvU * supp + myConvV
    if (is_half_gcf) {
      int index = uvo_shared[i].gcf_layer_index;
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
        supportPixel = gcf[uvo_shared[i].gcf_layer_index][__layeroff];
    }

    if (myGridU != grid_point_u || myGridV != grid_point_v) {
      atomicAdd(&psfi[grid_point_u][grid_point_v], sum);
      sum = make_cuDoubleComplex(0.0, 0.0);
      grid_point_u = myGridU;
      grid_point_v = myGridV;
    }
    sum += supportPixel;
  }
  atomicAdd(&psfi[grid_point_u][grid_point_v], sum);
}
////////////////////////////////////

template <
    int grid_size

  , int timesteps
  , int channels
  , bool is_half_gcf
  >
__inline__ __device__
// grid must be initialized to 0s.
void gridKernel_scatter_kernel(
    int max_supp
  , const complexd * gcf[]
  , Double4c grid[grid_size][grid_size]
  , const Pregridded uvo_shared[timesteps * channels]
  , const Double4c vis_shared[timesteps * channels]
  , const int2 off_shared[timesteps * channels]
  ) {
  for (int i = threadIdx.x; i < max_supp * max_supp; i += blockDim.x) {
    int
        myU = i % max_supp
      , myV = i / max_supp
      ;
    gridKernel_scatter_kernel_small<
      grid_size

    , timesteps
    , channels
    , is_half_gcf
    > (max_supp, gcf, grid, uvo_shared, vis_shared, off_shared, myU, myV);
  }
}

template <
    int grid_size

  , int timesteps
  , int channels
  , bool is_half_gcf
  >
__inline__ __device__
// grid must be initialized to 0s.
void psfiKernel_scatter_kernel(
    int max_supp
  , const complexd * gcf[]
  , complexd psfi[grid_size][grid_size]
  , const Pregridded uvo_shared[timesteps * channels]
  , const int2 off_shared[timesteps * channels]
  ) {
  for (int i = threadIdx.x; i < max_supp * max_supp; i += blockDim.x) {
    int
        myU = i % max_supp
      , myV = i / max_supp
      ;
    psfiKernel_scatter_kernel_small<
      grid_size

    , timesteps
    , channels
    , is_half_gcf
    > (max_supp, gcf, psfi, uvo_shared, off_shared, myU, myV);
  }
}

template <
    int over
  , int grid_size

  , int timesteps
  , int channels
  , bool is_half_gcf
  >
__device__ __inline__ void addBaselineToGrid(
    double scale
  , double wstep
  , int max_supp
  , Double4c grid[grid_size][grid_size]
  , const complexd * gcf[]
  , const double3 uvw[timesteps * channels]
  , const Double4c vis[timesteps * channels]
  ) {
  __shared__ Pregridded uvo_shared[timesteps * channels];
  __shared__ Double4c vis_shared[timesteps * channels];
  __shared__ int2 off_shared[timesteps * channels];
  
  loadUVWIntoSharedMem<
      over
    , grid_size
    , timesteps
    , channels
    , is_half_gcf
    >(scale
    , wstep
    , max_supp
    , uvw
    , uvo_shared
    , off_shared
    );
  loadVisIntoSharedMem<
      timesteps
    , channels
    >(vis
    , vis_shared
    );
  syncthreads();
  gridKernel_scatter_kernel<
      grid_size
    , timesteps
    , channels
    , is_half_gcf
    >(max_supp
    , gcf
    , grid
    , uvo_shared
    , vis_shared
    , off_shared
  );
}

template <
    int over
  , int grid_size

  , int timesteps
  , int channels
  , bool is_half_gcf
  >
__device__ __inline__ void addBaselineToPsfi(
    double scale
  , double wstep
  , int max_supp
  , complexd psfi[grid_size][grid_size]
  , const complexd * gcf[]
  , const double3 uvw[timesteps * channels]
  , const Double4c vis[timesteps * channels]
  ) {
  // FIXME: should share shared memory with grid computation
  // Also that perhaps was a bug (we had each instantiation
  // had it's own shared memory copy).
  // FIXME: investigate this futher!
  __shared__ Pregridded uvo_shared[timesteps * channels];
  __shared__ int2 off_shared[timesteps * channels];
  
  loadUVWIntoSharedMem<
      over
    , grid_size
    , timesteps
    , channels
    , is_half_gcf
    >(scale
    , wstep
    , max_supp
    , uvw
    , uvo_shared
    , off_shared
    );
  syncthreads();
  psfiKernel_scatter_kernel<
      grid_size
    , timesteps
    , channels
    , is_half_gcf
    >(max_supp
    , gcf
    , psfi
    , uvo_shared
    , off_shared
  );
}

template <
    int over
  , int grid_size

  , int timesteps
  , int channels
  , bool is_half_gcf
  , bool use_permutations
  >
__device__ __inline__ void addBaselinesToGrid(
    double scale
  , double wstep
  , const BlWMap permutations[/* baselines */]
  , Double4c grid[grid_size][grid_size]
  , const complexd * gcf[]
  , const double3 uvw[/* baselines */][timesteps * channels]
  , const Double4c vis[/* baselines */][timesteps * channels]
  , int blOff
  ) {
  int bl = blockIdx.x + blOff;
  if (use_permutations) bl = permutations[bl].bl;
  int max_supp = get_supp(permutations[bl].wp);

  addBaselineToGrid<
      over
    , grid_size
    , timesteps
    , channels
    , is_half_gcf
    >(scale
    , wstep
    , max_supp
    , grid
    , gcf
    , uvw[bl]
    , vis[bl]
    );
}

template <
    int over
  , int grid_size

  , int timesteps
  , int channels
  , bool is_half_gcf
  , bool use_permutations
  >
__device__ __inline__ void addBaselinesToPsfi(
    double scale
  , double wstep
  , complexd psfi[grid_size][grid_size]
  , const complexd * gcf[]
  , const double3 uvw[/* baselines */][timesteps * channels]
  , int blOff
  ) {
  int bl = blockIdx.x + blOff;
  if (use_permutations) bl = permutations[bl].bl;
  int max_supp = get_supp(permutations[bl].wp);

  addBaselineToPsfi<
      over
    , grid_size
    , timesteps
    , channels
    , is_half_gcf
    >(scale
    , wstep
    , max_supp
    , psfi
    , gcf
    , uvw[bl]
    );
}

#define addBaselineToGrid(suff, ishalf)                           \
extern "C"                                                        \
__global__ void addBaselineToGrid##suff(                          \
    double scale                                                  \
  , double wstep                                                  \
  , int max_supp                                                  \
  , Double4c grid[GRID_SIZE][GRID_SIZE]                           \
  , const complexd * gcf[]                                        \
  , const double3 uvw[TIMESTEPS*CHANNELS]                         \
  , const Double4c vis[TIMESTEPS*CHANNELS]                        \
  ) {                                                             \
  addBaselineToGrid<OVER, GRID_SIZE, TIMESTEPS, CHANNELS, ishalf> \
    ( scale                                                       \
    , wstep                                                       \
    , max_supp                                                    \
    , grid                                                        \
    , gcf                                                         \
    , uvw                                                         \
    , vis                                                         \
    );                                                            \
}
addBaselineToGrid(HalfGCF, true)
addBaselineToGrid(FullGCF, false)

#define addBaselinesToGridSkaMid(suff, ishalf)                            \
extern "C"                                                                \
__global__ void addBaselinesToGridSkaMid##suff(                           \
    double scale                                                          \
  , double wstep                                                          \
  , const BlWMap permutations[/* baselines */]                            \
  , Double4c grid[GRID_SIZE][GRID_SIZE]                                   \
  , const complexd * gcf[]                                                \
  , const double3 uvw[/* baselines */][TIMESTEPS*CHANNELS]                \
  , const Double4c vis[/* baselines */][TIMESTEPS*CHANNELS]               \
  , int blOff                                                             \
  ) {                                                                     \
  addBaselinesToGrid<OVER, GRID_SIZE, TIMESTEPS, CHANNELS, ishalf, false> \
    ( scale                                                               \
    , wstep                                                               \
    , permutations                                                        \
    , grid                                                                \
    , gcf                                                                 \
    , uvw                                                                 \
    , vis                                                                 \
    , blOff                                                               \
    );                                                                    \
}
addBaselinesToGridSkaMid(HalfGCF, true)
addBaselinesToGridSkaMid(FullGCF, false)

#define addBaselinesToGridSkaMidUsingPermutations(suff, ishalf)          \
extern "C"                                                               \
__global__ void addBaselinesToGridSkaMidUsingPermutations##suff(         \
    double scale                                                         \
  , double wstep                                                         \
  , const BlWMap permutations[/* baselines */]                           \
  , Double4c grid[GRID_SIZE][GRID_SIZE]                                  \
  , const complexd * gcf[]                                               \
  , const double3 uvw[/* baselines */][TIMESTEPS*CHANNELS]               \
  , const Double4c vis[/* baselines */][TIMESTEPS*CHANNELS]              \
  , int blOff                                                            \
  ) {                                                                    \
  addBaselinesToGrid<OVER, GRID_SIZE, TIMESTEPS, CHANNELS, ishalf, true> \
    ( scale                                                              \
    , wstep                                                              \
    , permutations                                                       \
    , grid                                                               \
    , gcf                                                                \
    , uvw                                                                \
    , vis                                                                \
    , blOff                                                              \
    );                                                                   \
}
addBaselinesToGridSkaMidUsingPermutations(HalfGCF, true)
addBaselinesToGridSkaMidUsingPermutations(FullGCF, false)

#include "../GCF_new/scale_complex_by_dbl.cuh"

extern "C"
__global__ void  normalizeAndExtractPolarization(
    complexd dst_grid[GRID_SIZE][GRID_SIZE]
  , const complexd src_grid[GRID_SIZE][GRID_SIZE][4]
  , int pol
  )
{
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y;

  dst_grid[x][y] = cuMulComplexByDouble(src_grid[x][y][pol], 1.0/(GRID_SIZE*GRID_SIZE));
}
