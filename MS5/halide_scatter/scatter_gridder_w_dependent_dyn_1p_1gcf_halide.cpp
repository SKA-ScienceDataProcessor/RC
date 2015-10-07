#ifdef _MSC_VER
#include <vector>
#endif
#include <omp.h>

#include "aligned_malloc.h"
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

#pragma omp parallel
  {
    auto gcf_buf = mkInterleavedHalideBufs<2, double>();

    // FIXME: add an API
    gcf_buf[0].extent[2] = gcf_buf[1].extent[2] =
    gcf_buf[0].extent[3] = gcf_buf[1].extent[3] = over;

    buffer_t uvw_buf = mkHalideBuf<3, double>(ts_ch);
    buffer_t vis_buf = mkHalideBuf<2, double>(ts_ch);

    int siz = grid_size*grid_pitch;
    complexd * grid = grids + omp_get_thread_num() * siz;

    memset(grid, 0, sizeof(complexd) * siz);
    buffer_t grid_buf = mkHalideBufPadded<2>(__tod(grid), grid_size, grid_pitch-grid_size);

#pragma omp for schedule(dynamic,23)
    for(int bl = 0; bl < baselines; bl++){
      setHalideBuf(__tod(_uvw[bl], const), uvw_buf);
      setHalideBuf(__tod(_vis[bl], const), vis_buf);
      setInterleavedHalideBufs(gcf[bl_wp_map[bl].wp], gcf_buf);
      // FIXME: add an API
      gcf_buf[0].extent[0] = gcf_buf[0].extent[1] =
      gcf_buf[1].extent[0] = gcf_buf[1].extent[1] = bl_supps[bl];
      set_strides(&gcf_buf[0]);
      set_strides(&gcf_buf[1]);

      uvg11_full(
          scale
        , wstep
        , ts_ch
        , &uvw_buf
        , &vis_buf
        , bl_supps[bl]
        , &gcf_buf[0]
        , &gcf_buf[1]
        , &grid_buf
        );
    }
  }
}


inline
void addGrids(
    complexd dst[]
  , const complexd srcs[]
  , int nthreads
  , int grid_pitch
  , int grid_size
  )
{
  int siz = grid_size*grid_pitch;
#pragma omp parallel for
  for (int i = 0; size_t(i) < siz*sizeof(complexd)/__MMSIZE; i++) {
    __mdType sum = asMdpc(srcs)[i];
    // __m256d sum = _mm256_loadu_pd(reinterpret_cast<const double*>(as256pc(srcs)+i));

    for (int g = 1; g < nthreads; g ++)
      sum = _mm_add_pd(sum, asMdpc(srcs + g * siz)[i]);

    asMdp(dst)[i] = sum;
  }
}

void gridKernel_scatter_halide_full(
    double scale
  , double wstep
  , int baselines
  , const BlWMap bl_wp_map[/* baselines */]
  , const int bl_supps[/* baselines */] // computed from previous
  , complexd grid[]
    // w-planes length array of pointers
    // to [over][over][gcf_supp][gcf_supp] arrays
  , const complexd * gcf[]
  , const Double3 * uvw[]
  , const complexd * vis[]
  , int ts_ch
  , int grid_pitch
  , int grid_size
  ) {
  int siz = grid_size*grid_pitch;
  int nthreads;

#pragma omp parallel
#pragma omp single
  nthreads = omp_get_num_threads();

  // Nullify incoming grid, allocate thread-local grids
  memset(grid, 0, sizeof(complexd) * siz);
  complexd * tmpgrids = alignedMallocArray<complexd>(siz * nthreads, 32);

  gridKernel_scatter_halide(
      scale
    , wstep
    , baselines
    , bl_wp_map
    , bl_supps
    , tmpgrids
    , gcf
    , uvw
    , vis
    , ts_ch
    , grid_pitch
    , grid_size
    );

  addGrids(grid, tmpgrids, nthreads, grid_pitch, grid_size);
  _aligned_free(tmpgrids);
}
