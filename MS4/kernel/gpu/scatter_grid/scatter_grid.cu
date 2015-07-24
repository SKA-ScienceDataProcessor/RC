
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
    char x, y;
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
extern "C" __global__ void scatter_grid_phaseRotate_kern
  ( complexd vis[]
  , const double3 uvw[]
  , int visibilities
  )
{
  for (int i = threadIdx.x; i < visibilities; i += blockDim.x) {
    vis[i] = rotw(vis[i], uvw[i].z);
  }
}

// Prepare positions for main gridding
extern "C" __global__ void scatter_grid_pregrid_kern
  ( double scale
  , double wstep
  , const double3 *uvws[]
  , const complexd *gcfs[]
  , const int gcf_supp[]
  , Pregridded *uvos[]
  , int max_supp
  , int visibilities
  , int grid_size
  )
{
  const double3 *uvw = uvws[blockIdx.x];
  Pregridded *uvo = uvos[blockIdx.x];
  for (int i = threadIdx.x; i < visibilities; i += blockDim.x) {

    // Scale and round uv so it gives us the top-left corner of the
    // where a GCF of the given size would get applied.
    short u = short(floor(uvw[i].x * scale))
        , v = short(floor(uvw[i].y * scale))
        , over_u = short(floor(over * (uvw[i].x * scale - u)))
        , over_v = short(floor(over * (uvw[i].y * scale - v)));
    uvo[i].u = u + grid_size/2 - max_supp/2;
    uvo[i].v = v + grid_size/2 - max_supp/2;
    uvo[i].x = uvo[i].u % max_supp;
    uvo[i].y = uvo[i].v % max_supp;

    // Determine GCF to use by w-plane and oversampling point. Note
    // that we need to re-centre the GCF if it is larger than
    // max_supp, as the gridder itself will only ever access a
    // max_supp*max_supp window of it.
    short w_plane = short(round(fabs(uvw[i].z / wstep)))
        , supp = gcf_supp[w_plane]
        , gcf_off = (supp - max_supp) / 2;
    uvo[i].gcf = gcfs[w_plane] + (over_u * over + over_v)*supp*supp
                               + gcf_off*supp + gcf_off;

    // gcf_supp carries both the GCF size as well as the sign for the
    // imaginary part. If it's negative, we are meant to use the
    // conjugate of the GCF pixel.
    uvo[i].gcf_supp = short(copysign(double(supp), uvw[i].z));
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
      if (grid_point_u >= 0 && grid_point_u < grid_size &&
          grid_point_v >= 0 && grid_point_v < grid_size) {
        atomicAdd(&grid[grid_point_u + grid_pitch*grid_point_v], sum);
      }
      // Switch to new point
      sum = make_cuDoubleComplex(0.0, 0.0);
      grid_point_u = myGridU;
      grid_point_v = myGridV;
    }

    // Load GCF pixel
    short supp = abs(uvo[i].gcf_supp);
    complexd px = uvo[i].gcf[myConvU * supp + myConvV];
    px.y = copysign(px.y, double(uvo[i].gcf_supp));

    // Sum up
    sum = cuCfma(px, vis[i], sum);
  }

  // Add remaining sum to grid
  if (grid_point_u >= 0 && grid_point_u < grid_size &&
      grid_point_v >= 0 && grid_point_v < grid_size) {
    atomicAdd(&grid[grid_point_u + grid_pitch*grid_point_v], sum);
  }
}

extern __shared__ __device__ complexd shared[];

extern "C" __global__ void scatter_grid_kern
  ( complexd grid[] // must be initialized to 0.
  , const Pregridded *uvos[]
  , const complexd *viss[]
  , int max_supp
  , int visibilities
  , int grid_size
  , int grid_pitch
  )
{

  // Copy arrays to shared memory.
  complexd *vis_shared = (complexd*)shared;
  Pregridded *uvo_shared = (Pregridded *)&shared[visibilities];
  for (int i = threadIdx.x; i < visibilities; i += blockDim.x) {
    vis_shared[i] = viss[blockIdx.x][i];
    uvo_shared[i] = uvos[blockIdx.x][i];
  }
  __syncthreads();

  // Start gridding points
  for (int i = threadIdx.x; i < max_supp * max_supp; i += blockDim.x) {
    int myU = i % max_supp
      , myV = i / max_supp;
    scatter_grid_point(max_supp, grid, uvo_shared, vis_shared, myU, myV, visibilities, grid_size, grid_pitch);
  }
}
