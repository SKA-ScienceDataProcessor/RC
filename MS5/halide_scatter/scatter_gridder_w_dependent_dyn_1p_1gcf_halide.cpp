#ifdef _MSC_VER
#include <vector>
#endif

#include <cstring>
#include <array>

#if defined _OPENMP
#include <omp.h>
#else
#define omp_get_thread_num()  0
#endif

#include "uvg11_full.h"
#include "mkHalideBuf.h"

#include "scatter_gridder_w_dependent_dyn_1p_1gcf_halide.h"

using namespace std;

#define __tod(a, ...) reinterpret_cast<__VA_ARGS__ double *>(a)

const int over = 8;
void gridKernel_scatter_halide(
    double scale
  , double wstep
  , int baselines
  , const BlWMap bl_wp_map[/* baselines */]
  , const int bl_supps[/* baselines */] // computed from previous
  , complexd grids[]
    // w-planes length array of pointers
    // to [over][over][gcf_supp][gcf_supp] arrays
  , const complexd * gcf[]
  , const Double3 * _uvw[]
  , const complexd * _vis[]
  , int ts_ch
  , int grid_pitch
  , int grid_size
  ) {

  auto gcf_buf = mkInterleavedHalideBufs<2, double>();

  // FIXME: add an API
  gcf_buf[0].extent[2] = gcf_buf[1].extent[2] =
  gcf_buf[0].extent[3] = gcf_buf[1].extent[3] = over;

  buffer_t uvw_buf = mkHalideBuf<3, double>(ts_ch);
  buffer_t vis_buf = mkHalideBuf<2, double>(ts_ch);

#pragma omp parallel
  {
    int siz = grid_size*grid_pitch;
    complexd * grid = grids + omp_get_thread_num() * siz;

    memset(grid, 0, sizeof(complexd) * siz);
    buffer_t grid_buf = mkHalideBufPadded<2>(__tod(grid), grid_size, grid_pitch-grid_size);

#pragma omp for schedule(dynamic,23)
    for(int bl = 0; bl < baselines; bl++){
      setHalideBuf(__tod(_uvw[bl], const), uvw_buf);
      setHalideBuf(__tod(_vis[bl], const), vis_buf);
      int curr_wp = bl_wp_map[bl].wp;
      setInterleavedHalideBufs(gcf[curr_wp], gcf_buf);
      int supp = bl_supps[curr_wp];
      // FIXME: add an API
      gcf_buf[0].extent[0] = gcf_buf[0].extent[1] =
      gcf_buf[1].extent[0] = gcf_buf[1].extent[1] = supp;
      set_strides(&gcf_buf[0]);
      set_strides(&gcf_buf[1]);

      uvg11_full(
          scale
        , wstep
        , ts_ch
        , &uvw_buf
        , &vis_buf
        , supp
        , &gcf_buf[0]
        , &gcf_buf[1]
        , &grid_buf
        );
    }
  }
}
