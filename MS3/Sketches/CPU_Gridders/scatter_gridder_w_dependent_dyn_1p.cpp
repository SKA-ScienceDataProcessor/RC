#include <cstring>

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
#include "OskarBinReader.h"
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

// We could simply use pointer-to-function template
// but most C++ compilers seem to produce worse code
// in such a case. Thus we wrap it in a class.
template <
    int over
  , bool do_mirror
  , typename Inp
  > struct cvt {};

template <
    int over
  , bool do_mirror
  > struct cvt<over, do_mirror, Pregridded> {
  static void pre(double, double, Pregridded inp, Pregridded & outpr, int) {outpr = inp;}
};

template <
    int over
  , bool do_mirror
  > struct cvt<over, do_mirror, Double3> {
  static void pre(double scale, double wstep, Double3 inp, Pregridded & outpr, int grid_size) {
    pregridPoint<over, do_mirror>(scale, wstep, inp, outpr, grid_size);
  }
};

template <
    int over
  , bool is_half_gcf
  , bool use_permutations

  , typename Inp
  >
// grid must be initialized to 0s.
void gridKernel_scatter(
    double scale
  , double wstep
  , int baselines
  , const BlWMap permutations[/* baselines */]
  , GRID_MOD complexd grids[]
    // We have a [w_planes][over][over]-shaped array of pointers to
    // variable-sized gcf layers, but we precompute (in pregrid)
    // exact index into this array, thus we use plain pointer here
  , const complexd * gcf[]
  , const Inp _uvw[]
  , VIS_MOD complexd _vis[]
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
#endif
    __ACC(complexd, grid, grid_pitch);

#pragma omp for schedule(dynamic)
    for(int bl0 = 0; bl0 < baselines; bl0++) {
      int bl;
      if (use_permutations) bl = permutations[bl0].bl;
      else bl = bl0;
      int max_supp_here;
      max_supp_here = get_supp(permutations[bl].wp);

      int off;
      off = bl*ts_ch;
      const Inp * uvw;
      uvw = _uvw + off;

      Pregridded pa[ts_ch];
#ifndef __DEGRID
      // Rotation
      // VLA requires "--std=gnu..." extension
      complexd vis[ts_ch];
#else
      complexd * vis;
      vis = _vis + off;
#endif
      for(int n=0; n<ts_ch; n++){
#ifndef __DEGRID
        vis[n] = rotw(_vis[off + n], uvw[n].w);
#else
        vis[n] = {0.0, 0.0};
#endif
        cvt<over, is_half_gcf, Inp>::pre(scale, wstep, uvw[n], pa[n], grid_size);
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
            vis[i] += grid[gsu][gsv] * supportPixel;
#endif
          }
        }
      }
#ifdef __DEGRID
      for(int n=0; n<ts_ch; n++)
        vis[n] = rotw(vis[n], uvw[n].w);
#endif
    }
  }
}

#ifndef __DEGRID
template <
    int over
  , bool is_half_gcf
  , bool use_permutations

  , typename Inp
  >
// grid must be initialized to 0s.
void gridKernel_scatter_full(
    double scale
  , double wstep
  , int baselines
  , const BlWMap permutations[/* baselines */]
  , complexd grid[]
    // We have a [w_planes][over][over]-shaped array of pointers to
    // variable-sized gcf layers, but we precompute (in pregrid)
    // exact index into this array, thus we use plain pointer here
  , const complexd * gcf[]
  , const Inp uvw[]
  , const complexd vis[]
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
    , use_permutations

    , Inp
    >(scale, wstep, baselines, permutations, tmpgrids, gcf, uvw, vis, ts_ch, grid_pitch, grid_size);
  addGrids(grid, tmpgrids, nthreads, grid_pitch, grid_size);
  free(tmpgrids);
#else
  gridKernel_scatter<
      over
    , is_half_gcf
    , use_permutations

    , Inp
    >(scale, wstep, baselines, permutations, grid, gcf, uvw, vis, ts_ch, grid_pitch, grid_size);
#endif
}

#define gridKernelCPU(hgcfSuff, isHgcf, permSuff, isPerm)  \
extern "C"                                                 \
void gridKernelCPU##hgcfSuff##permSuff(                    \
    double scale                                           \
  , double wstep                                           \
  , int baselines                                          \
  , const BlWMap permutations[/* baselines */]             \
  , complexd grid[]                                        \
  , const complexd * gcf[]                                 \
  , const Double3 uvw[]                                    \
  , const complexd vis[]                                   \
  , int ts_ch                                              \
  , int grid_pitch                                         \
  , int grid_size                                          \
  ){                                                       \
  gridKernel_scatter_full<OVER, isHgcf, isPerm>            \
    ( scale, wstep, baselines, permutations                \
    , grid, gcf, uvw, vis, ts_ch, grid_pitch, grid_size);  \
}

gridKernelCPU(HalfGCF, true, Perm, true)
gridKernelCPU(HalfGCF, true, , false)
gridKernelCPU(FullGCF, false, Perm, true)
gridKernelCPU(FullGCF, false, , false)

// Normalization is done inplace!
extern "C"
void normalize(
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

#define deGridKernelCPU(hgcfSuff, isHgcf, permSuff, isPerm) \
extern "C"                                                  \
void deGridKernelCPU##hgcfSuff##permSuff(                   \
    double scale                                            \
  , double wstep                                            \
  , int baselines                                           \
  , const BlWMap permutations[/* baselines */]              \
  , const complexd grid[]                                   \
  , const complexd * gcf[]                                  \
  , const Double3 uvw[]                                     \
  , complexd vis[]                                          \
  , int ts_ch                                               \
  , int grid_pitch                                          \
  , int grid_size                                           \
  ){                                                        \
  gridKernel_scatter<OVER, isHgcf, isPerm>                  \
    ( scale, wstep, baselines, permutations                 \
    , grid, gcf, uvw, vis, ts_ch, grid_pitch, grid_size);   \
}

deGridKernelCPU(HalfGCF, true, Perm, true)
deGridKernelCPU(HalfGCF, true, , false)
deGridKernelCPU(FullGCF, false, Perm, true)
deGridKernelCPU(FullGCF, false, , false)

#endif
