#include "scatter_gridder.h"
#include "atomic_add.h"
#include "OskarBinReader.h"

#include <math_functions.h>

struct Pregridded
{
  short u;
  short v;
  char gcf_layer_w_plane;
  char gcf_layer_over;
  short gcf_layer_supp;
};

template <
    int over
  , int w_planes
  , int grid_size

  , int timesteps
  , int channels
  >
__inline__ __device__ void loadIntoSharedMem (
    double scale
  , int max_supp // needed for u v translation
  , const double3 uvw[timesteps * channels]
  , const Double4c vis[timesteps * channels]
  , Pregridded uvo_shared[timesteps * channels]
  , Double4c vis_shared[timesteps * channels]
  ) {
  for (int i = threadIdx.x; i < timesteps * channels; i += blockDim.x) {
    double3 coords = uvw[i];
    coords.x *= scale;
    coords.y *= scale;
    coords.z *= scale;
    int
        // We additionally translate these u v by -max_supp/2
        // because gridding procedure translate them back
        u = round(coords.x) + grid_size/2 - max_supp/2
      , v = round(coords.y) + grid_size/2 - max_supp/2
      , over_u = round(over * (coords.x - u))
      , over_v = round(over * (coords.y - v))
      , w_plane = round (coords.z / (w_planes/2))
      ;
    // uvo_shared[i] = {short(u), short(v), (char)w_plane, (char)(over_u * over + over_v), (short)get_supp(w_plane)};
    uvo_shared[i].u = short(u);
    uvo_shared[i].v = short(v);
    uvo_shared[i].gcf_layer_w_plane = (char)w_plane;
    uvo_shared[i].gcf_layer_over = (char)(over_u * over + over_v);
    uvo_shared[i].gcf_layer_supp = (short)get_supp(w_plane);
    vis_shared[i] = vis[i];
  }
}

template <
    int grid_size
  , int w_planes

  , int timesteps
  , int channels
  >
__inline__ __device__
// grid must be initialized to 0s.
void gridKernel_scatter_kernel_small(
    int max_supp
    // Should be passed 0-centered,
    // i.e. with 0-index in the middle
  , const complexd * gcf_layer[w_planes]
  , Double4c grid[grid_size][grid_size]
  , const Pregridded uvo_shared[timesteps * channels]
  , const Double4c vis[timesteps * channels]
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
    myConvU = myU - u % max_supp;
    if (myConvU < 0) myConvU += max_supp;
    myConvV = myV - v % max_supp;
    if (myConvV < 0) myConvV += max_supp;
    // This translates points by max_supp/2
    // returning them back to normal (they were translates by -max_supp/2 before)
    myGridU = u + myConvU;
    myGridV = v + myConvV;

    int supp = uvo_shared[i].gcf_layer_supp;

    int wplane, wplane_m;
    wplane = uvo_shared[i].gcf_layer_w_plane;
    if (wplane < 0) wplane_m = -wplane; else wplane_m = wplane;
    complexd supportPixel =
      gcf_layer[wplane_m]
        [(uvo_shared[i].gcf_layer_over * supp + myConvU) * supp + myConvV];
    // if mirrored make conjugation
    if (wplane < 0)
      supportPixel.y = - supportPixel.y;

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
    #define __ADD_SUPP(pol) sum##pol = cuCfma(supportPixel, vis[i].pol, sum##pol)
    __ADD_SUPP(XX);
    __ADD_SUPP(XY);
    __ADD_SUPP(YX);
    __ADD_SUPP(YY);
  }
  atomicAdd(&grid[grid_point_u][grid_point_v], sumXX, sumXY, sumYX, sumYY);
}

template <
    int grid_size
  , int w_planes

  , int timesteps
  , int channels
  >
__inline__ __device__
// grid must be initialized to 0s.
void gridKernel_scatter_kernel(
    int max_supp
  , const complexd * gcf_layer[w_planes]
  , Double4c grid[grid_size][grid_size]
  , const Pregridded uvo_shared[timesteps * channels]
  , const Double4c vis[timesteps * channels]
  ) {
  for (int i = threadIdx.x; i < max_supp * max_supp; i += blockDim.x) {
    int
        myU = i % max_supp
      , myV = i / max_supp
      ;
    gridKernel_scatter_kernel_small<
      grid_size
    , w_planes

    , timesteps
    , channels
    > (max_supp, gcf_layer, grid, uvo_shared, vis, myU, myV);
  }
}

template <
    int over
  , int w_planes
  , int grid_size

  , int timesteps
  , int channels
  >
__device__ __inline__ void addBaselineToGrid(
    double scale
  , int max_supp
  , Double4c grid[grid_size][grid_size]
  , const complexd * gcf_layers[w_planes]
  , const double3 uvw[timesteps * channels]
  , const Double4c vis[timesteps * channels]
  ) {
  __shared__ Pregridded uvo_shared[timesteps * channels];
  __shared__ Double4c vis_shared[timesteps * channels];
  
  loadIntoSharedMem<
      over
    , w_planes
    , grid_size
    , timesteps
    , channels
    >(scale
    , max_supp
    , uvw
    , vis
    , uvo_shared
    , vis_shared
    );
  syncthreads();
  gridKernel_scatter_kernel<
      grid_size
    , w_planes
    , timesteps
    , channels
    >(max_supp
    , gcf_layers
    , grid
    , uvo_shared
    , vis_shared
  );
}

template <
    int over
  , int w_planes
  , int grid_size

  , int baselines
  , int timesteps
  , int channels
  , bool use_permutations
  >
__device__ __inline__ void addBaselinesToGrid(
    double scale
  , const BlWMap permutations[baselines]
  , Double4c grid[grid_size][grid_size]
  , const complexd * gcf_layers[w_planes]
  , const double3 uvw[baselines][timesteps * channels]
  , const Double4c vis[baselines][timesteps * channels]
  ) {
  int bl = blockIdx.x;
  if (use_permutations) bl = permutations[blockIdx.x].bl;
  int max_supp = get_supp(permutations[bl].wp);

  addBaselineToGrid<
      over
    , w_planes
    , grid_size
    , timesteps
    , channels
    >(scale
    , max_supp
    , grid
    , gcf_layers
    , uvw[bl]
    , vis[bl]
    );
}

#define GRID_SIZE 4096
#define OVER 8
#define BASELINES 32131
#define WPLANES 63
#define TIMESTEPS 200
#define CHANNELS 1

__global__ void addBaselineToGrid(
    double scale
  , int max_supp
  , Double4c grid[GRID_SIZE][GRID_SIZE]
  , const complexd * gcf_layers[WPLANES]
  , const double3 uvw[TIMESTEPS]
  , const Double4c vis[TIMESTEPS]
  ) {
  addBaselineToGrid<OVER, WPLANES, GRID_SIZE, TIMESTEPS, CHANNELS>
    ( scale
    , max_supp
    , grid
    , gcf_layers
    , uvw
    , vis
    );
}

__global__ void addBaselinesToGridSkaMid(
    double scale
  , const BlWMap permutations[BASELINES]
  , Double4c grid[GRID_SIZE][GRID_SIZE]
  , const complexd * gcf_layers[WPLANES]
  , const double3 uvw[BASELINES][TIMESTEPS]
  , const Double4c vis[BASELINES][TIMESTEPS]
  ) {
  addBaselinesToGrid<OVER, WPLANES, GRID_SIZE, BASELINES, TIMESTEPS, CHANNELS, false>
    ( scale
    , permutations
    , grid
    , gcf_layers
    , uvw
    , vis
    );
}

__global__ void addBaselinesToGridSkaMidWithUsingPermutations(
    double scale
  , const BlWMap permutations[BASELINES]
  , Double4c grid[GRID_SIZE][GRID_SIZE]
  , const complexd * gcf_layers[WPLANES]
  , const double3 uvw[BASELINES][TIMESTEPS]
  , const Double4c vis[BASELINES][TIMESTEPS]
  ) {
  addBaselinesToGrid<OVER, WPLANES, GRID_SIZE, BASELINES, TIMESTEPS, CHANNELS, true>
    ( scale
    , permutations
    , grid
    , gcf_layers
    , uvw
    , vis
    );
}

__global__ void  extractPolarization(
    int pol
  , complexd dst_grid[GRID_SIZE][GRID_SIZE]
  , const complexd src_grid[GRID_SIZE][GRID_SIZE][4]
  )
{
  const int
      x = blockIdx.x * blockDim.x + threadIdx.x
    , y = blockIdx.y * blockDim.y + threadIdx.y;

  dst_grid[x][y] = src_grid[x][y][pol];
}
