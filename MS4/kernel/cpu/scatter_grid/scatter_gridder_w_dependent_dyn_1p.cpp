#include <cstring>
#ifdef __clang__
#include <vector>
#endif

#if defined _OPENMP
#include <omp.h>
#else
#define omp_get_thread_num()  0
#endif

#ifdef _MSC_VER
#pragma warning(disable:4127)
#endif
#define __DYN_GRID_SIZE
#include "common.h"
#include "metrix.h"
#include "aligned_malloc.h"

#define as256p(p) (reinterpret_cast<__m256d*>(p))
#define as256pc(p) (reinterpret_cast<const __m256d*>(p))

#ifndef __DEGRID
#define GRID_MOD
#define VIS_MOD const
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
  for (unsigned int i = 0; i < siz*sizeof(complexd)/(256/8); i++) {
    __m256d sum = as256pc(srcs)[i];
    // __m256d sum = _mm256_loadu_pd(reinterpret_cast<const double*>(as256pc(srcs)+i));

    for (int g = 1; g < nthreads; g ++)
      sum = _mm256_add_pd(sum, as256pc(srcs + g * siz)[i]);

    as256p(dst)[i] = sum;
  }
}
#else
#define GRID_MOD const
#define VIS_MOD
#endif

template <
    int over
  , bool is_half_gcf
  >
// grid must be initialized to 0s.
void gridKernel_scatter(
    double scale
  , double wstep
  , int baselines
  , const int gcf_supps[/* baselines */]
  , GRID_MOD complexd grids[]
    // We have a [w_planes][over][over]-shaped array of pointers to
    // variable-sized gcf layers, but we precompute (in pregrid)
    // exact index into this array, thus we use plain pointer here
  , const complexd * gcf[]
  , const Double3 * _uvw[]
  , VIS_MOD complexd * _vis[]
  , int ts_ch
  , int grid_pitch
  , int grid_size
  ) {
  int siz = grid_size*grid_pitch;
#pragma omp parallel
  {
    GRID_MOD complexd * _grid = grids + omp_get_thread_num() * siz;
#ifndef __DEGRID
    memset(_grid, 0, sizeof(complexd) * siz);
#else
    memset(_vis, 0, sizeof(complexd) * baselines * ts_ch);
#endif
    __ACC(complexd, grid, grid_pitch);

#pragma omp for schedule(dynamic)
    for(int bl = 0; bl < baselines; bl++) {
      int max_supp_here;
      max_supp_here = gcf_supps[bl];

      const Double3 * uvw;
      uvw = _uvw[bl];

      // VLA requires "--std=gnu..." extension
      Pregridded pa[ts_ch];
#ifndef __DEGRID
      // Clang doesn't allow non-POD types in VLAs,
      //  thus we use much more heavyweight vector here.
      #ifndef __clang__
      complexd vis[ts_ch];
      #else
      std::vector<complexd> vis(ts_ch);
      #endif
#endif
      for(int n=0; n<ts_ch; n++){
#ifndef __DEGRID
        vis[n] = rotw(_vis[bl][n], uvw[n].w);
#endif
        pregridPoint<over, is_half_gcf>(scale, wstep, uvw[n], pa[n], grid_size);
      }
#ifdef __DEGRID
      complexd * vis;
      vis = _vis[bl];
#endif
      for (int su = 0; su < max_supp_here; su++) { // Moved from 2-levels below according to Romein
        for (int i = 0; i < ts_ch; i++) {
          Pregridded p = pa[i];
          for (int sv = 0; sv < max_supp_here; sv++) {
            // Don't forget our u v are already translated by -max_supp_here/2
            int gsu, gsv;
            gsu = p.u + su;
            gsv = p.v + sv;

            complexd supportPixel;
            #define __layeroff su * max_supp_here + sv
            if (is_half_gcf) {
              int index;
              index = p.gcf_layer_index;
              // Negative index indicates that original w was mirrored
              // and we shall negate the index to obtain correct
              // offset *and* conjugate the result.
              if (index < 0) {
                supportPixel = conj(gcf[-index][__layeroff]);
              } else {
                supportPixel = gcf[index][__layeroff];
              }
            } else {
                supportPixel = gcf[p.gcf_layer_index][__layeroff];
            }
#ifndef __DEGRID
            grid[gsu][gsv] += vis[i] * supportPixel;
#else
            vis[i] += rotw(grid[gsu][gsv] * supportPixel, uvw[i].w);
#endif
          }
        }
      }
    }
  }
}

#ifndef __DEGRID
template <
    int over
  , bool is_half_gcf
  >
// grid must be initialized to 0s.
void gridKernel_scatter_full(
    double scale
  , double wstep
  , int baselines
  , const int gcf_supps[/* baselines */]
  , complexd grid[]
    // We have a [w_planes][over][over]-shaped array of pointers to
    // variable-sized gcf layers, but we precompute (in pregrid)
    // exact index into this array, thus we use plain pointer here
  , const complexd * gcf[]
  , const Double3 * uvw[]
  , const complexd * vis[]
  , int ts_ch
  , int grid_pitch
  , int grid_size
  ) {
#if defined _OPENMP
  int siz = grid_size*grid_pitch;
  int nthreads;

#pragma omp parallel
#pragma omp single
  nthreads = omp_get_num_threads();

  // Nullify incoming grid, allocate thread-local grids
  memset(grid, 0, sizeof(complexd) * siz);
  complexd * tmpgrids = alignedMallocArray<complexd>(siz * nthreads, 32);
  
  gridKernel_scatter<
      over
    , is_half_gcf
    >(scale, wstep, baselines, gcf_supps, tmpgrids, gcf, uvw, vis, ts_ch, grid_pitch, grid_size);
  addGrids(grid, tmpgrids, nthreads, grid_pitch, grid_size);
  free(tmpgrids);
#else
  gridKernel_scatter<
      over
    , is_half_gcf
    >(scale, wstep, baselines, gcf_supps, grid, gcf, uvw, vis, ts_ch, grid_pitch, grid_size);
#endif
}

#define gridKernelCPU(hgcfSuff, isHgcf)                   \
extern "C"                                                \
void gridKernelCPU##hgcfSuff(                             \
    double scale                                          \
  , double wstep                                          \
  , int baselines                                         \
  , const int gcf_supps[/* baselines */]                  \
  , complexd grid[]                                       \
  , const complexd * gcf[]                                \
  , const Double3 * uvw[]                                 \
  , const complexd * vis[]                                \
  , int ts_ch                                             \
  , int grid_pitch                                        \
  , int grid_size                                         \
  ){                                                      \
  gridKernel_scatter_full<OVER, isHgcf>                   \
    ( scale, wstep, baselines, gcf_supps                  \
    , grid, gcf, uvw, vis, ts_ch, grid_pitch, grid_size); \
}

gridKernelCPU(HalfGCF, true)
gridKernelCPU(FullGCF, false)

// Normalization is done inplace!
extern "C"
void normalize(
    int n
  , complexd src[]
  , int grid_pitch
  , int grid_size
  )
{
  int siz = grid_size*grid_pitch;
  double norm = 1.0/double(siz);
#pragma omp parallel for
  for (int i = 0; i < siz; i++) {
    src[i] *= norm;
  }
}

#else

#define deGridKernelCPU(hgcfSuff, isHgcf)                 \
extern "C"                                                \
void deGridKernelCPU##hgcfSuff(                           \
    double scale                                          \
  , double wstep                                          \
  , int baselines                                         \
  , const int gcf_supps[/* baselines */]                  \
  , const complexd grid[]                                 \
  , const complexd * gcf[]                                \
  , const Double3 * uvw[]                                 \
  , complexd * vis[]                                      \
  , int ts_ch                                             \
  , int grid_pitch                                        \
  , int grid_size                                         \
  ){                                                      \
  gridKernel_scatter<OVER, isHgcf>                        \
    ( scale, wstep, baselines, gcf_supps                  \
    , grid, gcf, uvw, vis, ts_ch, grid_pitch, grid_size); \
}

deGridKernelCPU(HalfGCF, true)
deGridKernelCPU(FullGCF, false)

#endif
