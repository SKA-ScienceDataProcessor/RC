#include <cstring>
#include <vector>

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
        // Rotation is factored out as a separate kernel
        // vis[n] = rotw(_vis[bl][n], uvw[n].w * scale);
        vis[n] = _vis[bl][n];
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
            if (gsu < 0 || gsu >= grid_size || gsv < 0 || gsv >= grid_size) continue;

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
      // Rotation is factored out as a separate kernel
      // for(int n=0; n<ts_ch; n++)
      //   vis[n] = rotw(vis[n], uvw[n].w * scale);
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

void grid0(
    const Double3 uvw[]
  , const complexd vis[]
  , complexd grid[]
  , double scale
  , int baselines_ts_ch
  , int grid_size
  ){
  int
      last = grid_size * grid_size
    , trans = grid_size / 2 * (grid_size + 1)
    ;
  for(int i = 0; i < baselines_ts_ch; i++, uvw++, vis++) {
    int u, v;
    u = int(round(uvw->u * scale));
    v = int(round(uvw->v * scale));
    int n, ng;
    n = u*grid_size+v;
	ng = n + trans;
    if (ng >= 0 && ng < last)
      grid[n] += *vis;
  }
}

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

// Sadly our pregridder is combined with
//  the gridder thus we have to perform
//  some duplicate work ...
void reweight(
    const Double3 uvw[]
  ,       complexd vis[]
  , double scale
  , int baselines_ts_ch
  , int grid_size
  ){
  std::vector<int> count_grid_vec(grid_size * grid_size, 0);
  int * count_grid = count_grid_vec.data() + grid_size/2*(grid_size+1);
  // We cache rounded values here, not sure it is better than
  //   recalculating them during the second pass ...
  typedef std::pair<int, int> ipair;
  std::vector<ipair> pregrid(baselines_ts_ch);

  ipair * pp = pregrid.data();
  const Double3 * uvwp = uvw;
  for(int i = 0; i < baselines_ts_ch; i++, uvwp++, pp++) {
    int u, v;
    u = int(round(uvwp->u * scale));
    v = int(round(uvwp->v * scale));
    count_grid[u*grid_size+v]++;
    *pp = ipair(u, v);
  }
  complexd * visp = vis;
  pp = pregrid.data();
  for(int i = 0; i < baselines_ts_ch; i++, visp++) {
    ipair p;
    p = *pp++;
    *visp /= double(count_grid[p.first*grid_size+p.second]);
  }
}

void rotateCPU(
    const Double3 uvw[]
  ,       complexd vis[]
  , int baselines_ts_ch
  , double scale
  ){
  for(int i=0; i < baselines_ts_ch; i++, uvw++, vis++)
     *vis = rotw(*vis, uvw->w * scale);
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
