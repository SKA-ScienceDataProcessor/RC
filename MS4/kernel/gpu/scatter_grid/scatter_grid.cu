
#include <math_functions.h>
#include <math_constants.h>
#include <cuComplex.h>

typedef cuDoubleComplex complexd;

#include "atomic_add.h"

// Hard-coded configuration
const short over = 8;

// Pre-gridded visibility positions
struct Pregridded
{
    short u, v;
    short x, y;
    short conj;
    short gcf_supp;
    const complexd *gcf;
};

static __inline__ __device__ complexd rotw(complexd v, double w)
{
  double s, c;
  sincos(2.0 * CUDART_PI * w, &s, &c);
  return cuCmul(v, make_cuDoubleComplex(c, s));
}

// Visibility phase rotation
static __device__ void scatter_grid_phaseRotate
  ( complexd vis[]
  , const double3 uvw[]
  , int visibilities
  )
{
  for (int i = threadIdx.x; i < visibilities; i += blockDim.x) {
    // Add rotation
    vis[i] = rotw(vis[i], uvw[i].z);
  }
}
extern "C" __global__ void scatter_grid_phaseRotate_kern
  ( complexd vis[]
  , const double3 uvw[]
  , int visibilities
  )
{
  scatter_grid_phaseRotate(vis, uvw, visibilities);
}
// Prepare positions for main gridding
static __device__ void scatter_grid_pregrid
  ( double scale
  , double wstep
  , int max_supp
  , const double3 uvw[]
  , const complexd *gcfs[]
  , const int gcf_supp[]
  , Pregridded uvo[]
  , int visibilities
  , int grid_size
  )
{
  for (int i = threadIdx.x; i < visibilities; i += blockDim.x) {

    // Scale and round uv so it gives us the top-left corner of the
    // top-left corner where a maximum-size GCF would get applied.
    short u = short(floor(uvw[i].x * scale))
        , v = short(floor(uvw[i].y * scale))
        , over_u = short(floor(over * (uvw[i].x * scale - u)))
        , over_v = short(floor(over * (uvw[i].y * scale - v)));
    uvo[i].u = u + grid_size/2 - max_supp/2;
    uvo[i].v = v + grid_size/2 - max_supp/2;
    uvo[i].x = uvo[i].u % max_supp;
    uvo[i].y = uvo[i].v % max_supp;

    // Determine GCF to use
    short w_plane = short(round(fabs(uvw[i].z / wstep)))
        , supp = gcf_supp[w_plane];
    uvo[i].conj = short(copysign(1.0, uvw[i].z));
    uvo[i].gcf = gcfs[w_plane] + (over_u * over + over_v)*supp*supp;
    uvo[i].gcf_supp = supp;
  }
}
extern "C" __global__ void scatter_grid_pregrid_kern
  ( double scale
  , double wstep
  , const double3 *uvws[]
  , const complexd *gcfs[]
  , const int gcf_supp[]
  , Pregridded *uvo[]
  , int max_supp
  , int visibilities
  , int baselines
  , int grid_size
  )
{
  scatter_grid_pregrid(scale, wstep, max_supp, uvws[blockIdx.x], gcfs, gcf_supp, uvo[blockIdx.x], visibilities, grid_size);
}

static __inline__ __device__
void scatter_grid_point
  ( int max_supp
  , complexd grid[] // must be initialized to 0.
  , const Pregridded uvo[]
  , const complexd vis[]
  , int myU
  , int myV
  , int visibilities
  , int grid_size
  , int grid_pitch
  )
{

  // Current grid point and accumulated uv-grid value
  int grid_point_u = myU, grid_point_v = myV;
  complexd sum = {0, 0};

  for (int i = 0; i < visibilities; i++) {

    // Load pre-calculated positions
    int u = uvo[i].u, v = uvo[i].v;

    // Determine convolution point. This is basically just an
    // optimised way to calculate
    //   myConvU = (myU - u) % max_supp
    //   myConvV = (myV - v) % max_supp
    int myConvU = myU - uvo[i].x
      , myConvV = myV - uvo[i].y;
    if (myConvU < 0) myConvU += max_supp;
    if (myConvV < 0) myConvV += max_supp;

    // Determine grid point. Because of the above we know here that
    //   myGridU % max_supp = myU
    //   myGridV % max_supp = myV
    int myGridU = u + myConvU
      , myGridV = v + myConvV;

    // Grid point changed?
    if (myGridU != grid_point_u || myGridV != grid_point_v) {
      // Atomically add to grid. This is the bottleneck of this kernel.
      atomicAdd(&grid[grid_point_u + grid_pitch*grid_point_v], sum);
      // Switch to new point
      sum = make_cuDoubleComplex(0.0, 0.0);
      grid_point_u = myGridU;
      grid_point_v = myGridV;
    }

    // Load GCF pixel, taking care to re-centre if max_supp != supp.
    short supp = uvo[i].gcf_supp
        , supp_off = (supp - max_supp) / 2;
    complexd px = uvo[i].gcf[(myConvU+supp_off) * supp + myConvV+supp_off];
    px.y = copysign(px.y, double(uvo[i].conj));

    // Sum up
    sum = cuCfma(px, vis[i], sum);
  }

  // Add remaining sum to grid
  atomicAdd(&grid[grid_point_u + grid_pitch*grid_point_v], sum);
}

__device__ void scatter_grid
  ( int max_supp
  , complexd grid[] // must be initialized to 0.
  , const Pregridded uvo[]
  , const complexd vis[]
  , int visibilities
  , int grid_size
  , int grid_pitch
  )
{
  for (int i = threadIdx.x; i < max_supp * max_supp; i += blockDim.x) {
    int myU = i % max_supp
      , myV = i / max_supp;
    scatter_grid_point(max_supp, grid, uvo, vis, myU, myV, visibilities, grid_size, grid_pitch);
  }
}

extern "C" __global__ void scatter_grid_kern
  ( complexd grid[] // must be initialized to 0.
  , const Pregridded *uvo[]
  , const complexd *vis[]
  , int max_supp
  , int visibilities
  , int grid_size
  , int grid_pitch
  )
{
  scatter_grid(max_supp, grid, uvo[blockIdx.x], vis[blockIdx.x], visibilities, grid_size, grid_pitch);
}
