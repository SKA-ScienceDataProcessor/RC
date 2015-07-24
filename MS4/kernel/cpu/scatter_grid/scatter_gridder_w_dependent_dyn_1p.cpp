#include <cstring>
#if defined __clang__ || defined _MSC_VER
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

#include "metrix.h"
#include "aligned_malloc.h"

#include "scatter_gridder_w_dependent_dyn_1p.h"

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
  for (int i = 0; size_t(i) < siz*sizeof(complexd)/__MMSIZE; i++) {
    __mdType sum = asMdpc(srcs)[i];
    // __m256d sum = _mm256_loadu_pd(reinterpret_cast<const double*>(as256pc(srcs)+i));

    for (int g = 1; g < nthreads; g ++)
      sum = _mm_add_pd(sum, asMdpc(srcs + g * siz)[i]);

    asMdp(dst)[i] = sum;
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
  , const int bl_supps[/* baselines */]
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
  , int gcf_supps[]
  ) {
  int siz = grid_size*grid_pitch;
#pragma omp parallel
  {
    GRID_MOD complexd * _grid = grids + omp_get_thread_num() * siz;
#ifndef __DEGRID
    memset(_grid, 0, sizeof(complexd) * siz);
#endif
    __ACC(complexd, grid, grid_pitch);

#pragma omp for schedule(dynamic)
    for(int bl = 0; bl < baselines; bl++) {
      int max_supp_here;
      max_supp_here = bl_supps[bl];

      const Double3 * uvw;
      uvw = _uvw[bl];

      // VLA requires "--std=gnu..." extension
#ifndef _MSC_VER
      Pregridded pa[ts_ch];
      complexd * gcflp[ts_ch];
      int gcfsupp[ts_ch];
#else
      std::vector<Pregridded> pa(ts_ch);
      std::vector<complexd*> gcflp(ts_ch);
      std::vector<int> gcfsupp(ts_ch);
#endif
#ifndef __DEGRID
      // Clang doesn't allow non-POD types in VLAs,
      //  thus we use much more heavyweight vector here.
      #ifdef __GNUC__
      complexd vis[ts_ch];
      #else
      std::vector<complexd> vis(ts_ch);
      #endif
#else
      complexd * vis;
      vis = _vis[bl];
#endif
      for(int n=0; n<ts_ch; n++){
#ifndef __DEGRID
        // We use scaled w for GCF hence scale w here too.
        vis[n] = rotw(_vis[bl][n], uvw[n].w * scale);
#else
        vis[n] = {0.0, 0.0};
#endif
        pregridPoint<over, is_half_gcf>(scale, wstep, uvw[n], max_supp_here, pa[n], grid_size);
        int index;
        index = pa[n].gcf_layer_index;
        if (is_half_gcf) {
          if (index < 0)
            gcflp[n] = const_cast<complexd *>(gcf[-index]);
          else
            gcflp[n] = const_cast<complexd *>(gcf[index]);
        } else {
            gcflp[n] = const_cast<complexd *>(gcf[index]);
        }
        // Correction
        gcfsupp[n] = gcf_supps[pa[n].w_plane];
        gcflp[n] += (gcfsupp[n] - max_supp_here) / 2 * (gcfsupp[n] + 1);
      }
      for (int su = 0; su < max_supp_here; su++) { // Moved from 2-levels below according to Romein
        for (int i = 0; i < ts_ch; i++) {
          Pregridded p;
          p = pa[i];
          for (int sv = 0; sv < max_supp_here; sv++) {
            // Don't forget our u v are already translated by -max_supp_here/2
            int gsu, gsv;
            gsu = p.u + su;
            gsv = p.v + sv;

            complexd supportPixel;
            #define __layeroff su * gcfsupp[i] + sv
            if (is_half_gcf) {
              if (p.gcf_layer_index < 0) {
                supportPixel = conj(gcflp[i][__layeroff]);
              } else {
                supportPixel = gcflp[i][__layeroff];
              }
            } else {
                supportPixel = gcflp[i][__layeroff];
            }
#ifndef __DEGRID
            grid[gsu][gsv] += vis[i] * supportPixel;
#else
            vis[i] += grid[gsu][gsv] * supportPixel;
#endif
          }
        }
      }
#ifdef __DEGRID
      // We use scaled w for GCF hence scale w here too.
      for(int n=0; n<ts_ch; n++)
        vis[n] = rotw(vis[n], uvw[n].w * scale);
#endif
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
  , const int bl_supps[/* baselines */]
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
  , int gcf_supps[]
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
    >(scale, wstep, baselines, bl_supps, tmpgrids, gcf, uvw, vis, ts_ch, grid_pitch, grid_size, gcf_supps);
  addGrids(grid, tmpgrids, nthreads, grid_pitch, grid_size);
  _aligned_free(tmpgrids);
#else
  gridKernel_scatter<
      over
    , is_half_gcf
    >(scale, wstep, baselines, bl_supps, grid, gcf, uvw, vis, ts_ch, grid_pitch, grid_size, gcf_supps);
#endif
}

#define gridKernelCPU(hgcfSuff, isHgcf)                   \
void gridKernelCPU##hgcfSuff(                             \
    double scale                                          \
  , double wstep                                          \
  , int baselines                                         \
  , const int bl_supps[/* baselines */]                   \
  , complexd grid[]                                       \
  , const complexd * gcf[]                                \
  , const Double3 * uvw[]                                 \
  , const complexd * vis[]                                \
  , int ts_ch                                             \
  , int grid_pitch                                        \
  , int grid_size                                         \
  , int gcf_supps[]                                       \
  ){                                                      \
  gridKernel_scatter_full<OVER, isHgcf>                   \
    ( scale, wstep, baselines, bl_supps, grid, gcf        \
    , uvw, vis, ts_ch, grid_pitch, grid_size, gcf_supps); \
}

gridKernelCPU(HalfGCF, true)
gridKernelCPU(FullGCF, false)

// Normalization is done inplace!
void normalizeCPU(
    complexd src[]
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
void deGridKernelCPU##hgcfSuff(                           \
    double scale                                          \
  , double wstep                                          \
  , int baselines                                         \
  , const int bl_supps[/* baselines */]                   \
  , const complexd grid[]                                 \
  , const complexd * gcf[]                                \
  , const Double3 * uvw[]                                 \
  , complexd * vis[]                                      \
  , int ts_ch                                             \
  , int grid_pitch                                        \
  , int grid_size                                         \
  , int gcf_supps[]                                       \
  ){                                                      \
  gridKernel_scatter<OVER, isHgcf>                        \
    ( scale, wstep, baselines, bl_supps, grid, gcf        \
    , uvw, vis, ts_ch, grid_pitch, grid_size, gcf_supps); \
}

deGridKernelCPU(HalfGCF, true)
deGridKernelCPU(FullGCF, false)

#endif
