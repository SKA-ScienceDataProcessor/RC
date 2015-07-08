
#include <math_functions.h>
#include <math_constants.h>
#include <cuComplex.h>

typedef cuDoubleComplex complexd;

#include "atomic_add.h"

// Hard-coded configuration
const short over = 8;

static __inline__ __device__ complexd rotw(complexd v, double w)
{
  double s, c;
  sincos(2.0 * CUDART_PI * w, &s, &c);
  return cuCmul(v, make_cuDoubleComplex(c, s));
}

// Visibility phase rotation
extern "C" __device__ void scatter_grid_phaseRotate
  ( const complexd vis[]
  , const double3 uvw[]
  , complexd vis_out[]
  , int visibilities
  )
{
  for (int i = threadIdx.x; i < visibilities; i += blockDim.x) {
    // Add rotation
    vis_out[i] = rotw(vis[i], uvw[i].z);
  }
}

// GCF information
struct GCF
{
    const complexd *gcf;
    short supp;
};

// Pre-gridded visibility positions
struct Pregridded
{
    short u, v;
    short x, y;
    short conj;
    const complexd *gcf;
    short gcf_supp;
};

// Prepare positions for main gridding
extern "C" __device__ void scatter_grid_preparePos
  ( double scale
  , double wstep
  , int max_supp
  , const double3 uvw[]
  , const GCF gcfs[]
  , Pregridded uvo[]
  , int visibilities
  , int grid_size
  )
{

  for (int i = threadIdx.x; i < visibilities; i += blockDim.x) {

    // Scale and round uv so it gives us the top-left corner of the
    // top-left corner where a maximum-size GCF would get applied.
    short u = short(grid_size/2 + floor(uvw[i].x * scale) - max_supp/2)
        , v = short(grid_size/2 + floor(uvw[i].y * scale) - max_supp/2);
    uvo[i].u = u;
    uvo[i].v = v;
    uvo[i].x = u % max_supp;
    uvo[i].y = v % max_supp;

    // Determine GCF to use
    short w_plane = short(round(fabs(uvw[i].z / wstep)))
        , supp = gcfs[w_plane].supp
        , over_u = short(floor(over * (uvw[i].x - u)))
        , over_v = short(floor(over * (uvw[i].y - v)));
    uvo[i].conj = short(copysign(1.0, uvw[i].z));
    uvo[i].gcf = gcfs[w_plane].gcf + (over_u * over + over_v)*supp*supp;
    uvo[i].gcf_supp = supp;
  }
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
    complexd px = uvo[i].gcf[(myConvU+supp_off) *  + myConvV+supp_off];
    px.y = copysign(px.y, double(uvo[i].conj));

    // Sum up
    sum = cuCfma(px, vis[i], sum);
  }

  // Add remaining sum to grid
  atomicAdd(&grid[grid_point_u + grid_pitch*grid_point_v], sum);
}

extern "C" __device__ void scatter_grid
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
