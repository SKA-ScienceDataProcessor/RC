#define __DYN_GRID_SIZE
#include "common.h"
#include "metrix.h"
#include "atomic_add.h"
#include "OskarBinReader.h"

template <
    int over
  , bool do_mirror
  >
__inline__ __device__ void loadUVWIntoSharedMem (
    double scale
  , double wstep
  , int max_supp // needed for u v translation
  , const double3 uvw[]
  , Pregridded uvo_shared[]
  , int2 off_shared[]
  , int timesteps_x_channels
  , int grid_size
  ) {
  for (int i = threadIdx.x; i < timesteps_x_channels; i += blockDim.x) {
    // uvo_shared[i] is passed by reference and updated!
    pregridPoint<over, do_mirror>(scale, wstep, uvw[i], uvo_shared[i], grid_size);
    off_shared[i].x = uvo_shared[i].u % max_supp;
    off_shared[i].y = uvo_shared[i].v % max_supp;
  }
}

template <
    bool is_half_gcf
  >
__inline__ __device__
// psfi must be initialized to 0s.
void psfiKernel_scatter_kernel_small(
    int max_supp
    // For full-size GCF should be passed 0-centered,
    // i.e. with 0-index in the middle
  , const complexd * gcf[]
  , complexd _psfi[]
  , const Pregridded uvo_shared[]
  , const int2 off_shared[]
  , int myU
  , int myV
  , int timesteps_x_channels
  , int grid_size
  ) {
  __ACC(complexd, psfi, grid_size);
  complexd sum = {0, 0};
  int
      grid_point_u = 0
    , grid_point_v = 0;

  for (int i = 0; i < timesteps_x_channels; i++) {
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
    sum = cuCadd(supportPixel, sum);
  }
  atomicAdd(&psfi[grid_point_u][grid_point_v], sum);
}

template <
    bool is_half_gcf
  >
__inline__ __device__
// grid must be initialized to 0s.
void psfiKernel_scatter_kernel(
    int max_supp
  , const complexd * gcf[]
  , complexd psfi[]
  , const Pregridded uvo_shared[]
  , const int2 off_shared[]
  , int timesteps_x_channels
  , int grid_size
  ) {
  for (int i = threadIdx.x; i < max_supp * max_supp; i += blockDim.x) {
    int
        myU = i % max_supp
      , myV = i / max_supp
      ;
    psfiKernel_scatter_kernel_small<
      is_half_gcf
    > (max_supp, gcf, psfi, uvo_shared, off_shared, myU, myV, timesteps_x_channels, grid_size);
  }
}

template <
    int over
  , bool is_half_gcf
  >
__device__ __inline__ void addBaselineToPsfi(
    double scale
  , double wstep
  , int max_supp
  , complexd psfi[]
  , const complexd * gcf[]
  , const double3 uvw[]
  , int timesteps_x_channels
  , int grid_size
  ) {
  // NOTE: Don't forget to put timesteps_x_channels*16
  //   to kernel launch shared memory config.
  extern __shared__ Pregridded uvo_shared[];
  int2 * off_shared =  reinterpret_cast<int2 *>(uvo_shared + timesteps_x_channels);
  
  loadUVWIntoSharedMem<
      over
    , is_half_gcf
    >(scale
    , wstep
    , max_supp
    , uvw
    , uvo_shared
    , off_shared
    , timesteps_x_channels
    , grid_size
    );
  syncthreads();
  psfiKernel_scatter_kernel<
      is_half_gcf
    >(max_supp
    , gcf
    , psfi
    , uvo_shared
    , off_shared
    , timesteps_x_channels
    , grid_size
  );
}

template <
    int over
  , bool is_half_gcf
  , bool use_permutations
  >
__device__ __inline__ void addBaselinesToPsfi(
    double scale
  , double wstep
  , const BlWMap permutations[/* baselines */]
  , complexd psfi[]
  , const complexd * gcf[]
  , const double3 uvw[]
  , int blOff
  , int timesteps_x_channels
  , int grid_size
  ) {
  int bl = blockIdx.x + blOff;
  if (use_permutations) bl = permutations[bl].bl;
  int max_supp = get_supp(permutations[bl].wp);

  addBaselineToPsfi<
      over
    , is_half_gcf
    >(scale
    , wstep
    , max_supp
    , psfi
    , gcf
    , uvw + bl * timesteps_x_channels
    , timesteps_x_channels
    , grid_size
    );
}

#define addBaselinesToPsfiSkaMid(suff, ishalf)       \
extern "C"                                           \
__global__ void addBaselinesToPsfiSkaMid##suff(      \
    double scale                                     \
  , double wstep                                     \
  , const BlWMap permutations[/* baselines */]       \
  , complexd psfi[]                                  \
  , const complexd * gcf[]                           \
  , const double3 uvw[]                              \
  , int blOff                                        \
  , int timesteps_x_channels                         \
  , int grid_size                                    \
  ) {                                                \
  addBaselinesToPsfi<OVER, ishalf, false>            \
    ( scale                                          \
    , wstep                                          \
    , permutations                                   \
    , psfi                                           \
    , gcf                                            \
    , uvw                                            \
    , blOff                                          \
    , timesteps_x_channels                           \
    , grid_size                                      \
    );                                               \
}
addBaselinesToPsfiSkaMid(HalfGCF, true)
addBaselinesToPsfiSkaMid(FullGCF, false)

#define addBaselinesToPsfiSkaMidUsingPermutations(suff, ishalf)  \
extern "C"                                                       \
__global__ void addBaselinesToPsfiSkaMidUsingPermutations##suff( \
    double scale                                                 \
  , double wstep                                                 \
  , const BlWMap permutations[/* baselines */]                   \
  , complexd psfi[]                                              \
  , const complexd * gcf[]                                       \
  , const double3 uvw[]                                          \
  , int blOff                                                    \
  , int timesteps_x_channels                                     \
  , int grid_size                                                \
  ) {                                                            \
  addBaselinesToPsfi<OVER, ishalf, true>                         \
    ( scale                                                      \
    , wstep                                                      \
    , permutations                                               \
    , psfi                                                       \
    , gcf                                                        \
    , uvw                                                        \
    , blOff                                                      \
    , timesteps_x_channels                                       \
    , grid_size                                                  \
    );                                                           \
}
addBaselinesToPsfiSkaMidUsingPermutations(HalfGCF, true)
addBaselinesToPsfiSkaMidUsingPermutations(FullGCF, false)
