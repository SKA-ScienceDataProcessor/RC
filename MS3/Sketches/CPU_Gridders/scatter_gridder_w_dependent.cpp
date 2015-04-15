#include <cstring>

#if defined _OPENMP
#include <omp.h>
#else
#define omp_get_thread_num()  0
#endif

#ifdef _MSC_VER
#pragma warning(disable:4127)
#endif
#include "common.h"
#include "metrix.h"
#include "OskarBinReader.h"
#include "aligned_malloc.h"

#define as256p(p) (reinterpret_cast<__m256d*>(p))
#define as256pc(p) (reinterpret_cast<const __m256d*>(p))

template <int grid_size>
void addGrids(
    Double4c dst[grid_size][grid_size]
  , const Double4c srcs[][grid_size][grid_size]
  , int nthreads
  )
{
#pragma omp parallel for
  for (int i = 0; i < grid_size*grid_size*sizeof(Double4c)/(256/8); i++) {
    // __m256d sum = as256pc(srcs[0])[i];
    __m256d sum = _mm256_loadu_pd(reinterpret_cast<const double*>(as256pc(srcs[0])+i));

    for (int g = 1; g < nthreads; g ++)
      sum = _mm256_add_pd(sum, as256pc(srcs[g])[i]);

    as256p(dst)[i] = sum;
  }
}

// We could simply use pointer-to-function template
// but most C++ compilers seem to produce worse code
// in such a case. Thus we wrap it in a class.
template <
    int grid_size
  , int over
  , int w_planes
  , bool do_mirror
  , typename Inp
  > struct cvt {};

template <
    int grid_size
  , int over
  , int w_planes
  , bool do_mirror
  > struct cvt<grid_size, over, w_planes, do_mirror, Pregridded> {
  static void pre(double, double, Pregridded inp, Pregridded & outpr) {outpr = inp;}
};

template <
    int grid_size
  , int over
  , int w_planes
  , bool do_mirror
  > struct cvt<grid_size, over, w_planes, do_mirror, Double3> {
  static void pre(double scale, double wstep, Double3 inp, Pregridded & outpr) {
    pregridPoint<grid_size, over, w_planes, do_mirror>(scale, wstep, inp, outpr);
  }
};

template <
    int over
  , int w_planes
  , bool is_half_gcf
  , bool use_permutations

  , int baselines
  , int grid_size
  , int ts_ch
  , typename Inp
  >
// grid must be initialized to 0s.
void gridKernel_scatter(
    double scale
  , double wstep
  , const BlWMap permutations[baselines]
  , Double4c grids[][grid_size][grid_size]
    // We have a [w_planes][over][over]-shaped array of pointers to
    // variable-sized gcf layers, but we precompute (in pregrid)
    // exact index into this array, thus we use plain pointer here
  , const complexd * gcf[]
  , const Inp uvw[baselines][ts_ch]
  , const Double4c vis[baselines][ts_ch]
  ) {
#pragma omp parallel
  {
    auto grid = grids[omp_get_thread_num()];

#pragma omp for schedule(dynamic)
    for(int bl0 = 0; bl0 < baselines; bl0++) {
      int bl;
      if (use_permutations) bl = permutations[bl0].bl;
      else bl = bl0;
      int max_supp_here = get_supp(permutations[bl].wp);
      for (int sv = 0; sv < max_supp_here; sv++) { // Moved from 2-levels below according to Romein
        for (int i = 0; i < ts_ch; i++) {
            Pregridded p;
            cvt<grid_size, over, w_planes, is_half_gcf, Inp>::pre(scale, wstep, uvw[bl][i], p);

#ifdef __AVX__
          // We port Romein CPU code to doubles here (for MS2 we didn't)
          // vis0 covers XX and XY, vis1 -- YX and YY
          __m256d vis0, vis1;
          // Temporarily use unaligned load
          // vis0 = _mm256_load_pd((const double *) &vis[bl][i].XX);
          // vis1 = _mm256_load_pd((const double *) &vis[bl][i].YX);
          vis0 = _mm256_loadu_pd((const double *) &vis[bl][i].XX);
          vis1 = _mm256_loadu_pd((const double *) &vis[bl][i].YX);
#endif

          for (int su = 0; su <= max_supp_here; su++) {
          // NOTE: Romein writes about moving this to the very outer scope
          // (2 levels up) to make things more cache-friendly.
          // for (int sv = 0; sv < max_supp_here; sv++) {
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

#ifdef __AVX__
            __m256d weight_r, weight_i;
            weight_r = _mm256_set1_pd(supportPixel.real());
            weight_i = _mm256_set1_pd(supportPixel.imag());

            /* _mm256_permute_pd(x, 5) swaps adjacent doubles pairs of x:
               d0, d1, d2, d3 goes to d1, d0, d3, d2
             */
            #define __DO(p,f)                                     \
            __m256d * gridptr##p = (__m256d *) &grid[gsu][gsv].f; \
            __m256d tr##p = _mm256_mul_pd(weight_r, vis##p);      \
            __m256d ti##p = _mm256_mul_pd(weight_i, vis##p);      \
            __m256d tp##p = _mm256_permute_pd(ti##p, 5);          \
            __m256d t##p = _mm256_addsub_pd(tr##p, tp##p);        \
            _mm256_storeu_pd(reinterpret_cast<double*>(gridptr##p + p), _mm256_add_pd(gridptr##p[p], t##p));

            __DO(0, XX);
            __DO(1, YX);
#else
            #define __GRID_POL(pol) grid[gsu][gsv].pol += vis[bl][i].pol * supportPixel
            __GRID_POL(XX);
            __GRID_POL(XY);
            __GRID_POL(YX);
            __GRID_POL(YY);
#endif
          }
        }
      }
    }
  }
}


template <
    int over
  , int w_planes
  , bool is_half_gcf
  , bool use_permutations

  , int baselines
  , int grid_size
  , int ts_ch
  , typename Inp
  >
// grid must be initialized to 0s.
void gridKernel_scatter_full(
    double scale
  , double wstep
  , const BlWMap permutations[baselines]
  , Double4c grid[grid_size][grid_size]
    // We have a [w_planes][over][over]-shaped array of pointers to
    // variable-sized gcf layers, but we precompute (in pregrid)
    // exact index into this array, thus we use plain pointer here
  , const complexd * gcf[]
  , const Inp uvw[baselines][ts_ch]
  , const Double4c vis[baselines][ts_ch]
  ) {
  typedef Double4c GT[grid_size][grid_size];

#if defined _OPENMP
  int nthreads;

#pragma omp parallel
#pragma omp single
  nthreads = omp_get_num_threads();

  // Also nullify incoming grid.
  memset(grid, 0, sizeof(GT));
  GT * tmpgrids = alignedMallocArray<GT>(nthreads, 32);
  memset(tmpgrids, 0, nthreads * sizeof(GT));
  gridKernel_scatter<
      over
    , w_planes
    , is_half_gcf
    , use_permutations

    , baselines
    , grid_size
    , ts_ch
    , Inp
    >(scale, wstep, permutations, tmpgrids, gcf, uvw, vis);
  addGrids<grid_size>(grid, tmpgrids, nthreads);
  free(tmpgrids);
#else
  gridKernel_scatter<
      over
    , w_planes
    , is_half_gcf
    , use_permutations

    , baselines
    , grid_size
    , ts_ch
    , Inp
    >(scale, wstep, permutations, reinterpret_cast<GT*>(&grid), gcf, uvw, vis);
#endif
}

#define gridKernelCPU(hgcfSuff, isHgcf, permSuff, isPerm)                      \
extern "C"                                                                     \
void gridKernelCPU##hgcfSuff##permSuff(                                        \
    double scale                                                               \
  , double wstep                                                               \
  , const BlWMap permutations[BASELINES]                                       \
  , Double4c grid[GRID_SIZE][GRID_SIZE]                                        \
  , const complexd * gcf[]                                                     \
  , const Double3 uvw[BASELINES][TIMESTEPS*CHANNELS]                           \
  , const Double4c vis[BASELINES][TIMESTEPS*CHANNELS]                          \
  ){                                                                           \
  gridKernel_scatter_full<OVER, WPLANES, isHgcf, isPerm, BASELINES, GRID_SIZE> \
    (scale, wstep, permutations, grid, gcf, uvw, vis);                         \
}

gridKernelCPU(HalfGCF, true, Perm, true)
gridKernelCPU(HalfGCF, true, , false)
gridKernelCPU(FullGCF, false, Perm, true)
gridKernelCPU(FullGCF, false, , false)

template <int grid_size, int num_pols>
void normalizeAndExtractPolarization(
    int n
  , complexd dst[grid_size][grid_size]
  , const complexd src[grid_size][grid_size][num_pols]
  )
{
  const double norm = 1.0/double(grid_size * grid_size);
#pragma omp parallel for
  for (int i = 0; i < grid_size*grid_size; i++) {
    dst[0][i] = src[0][i][n] * norm;
  }
}

extern "C" void normalizeAndExtractPolarization(
    int n
  , complexd dst[GRID_SIZE][GRID_SIZE]
  , const complexd src[GRID_SIZE][GRID_SIZE][NUM_POL]
  ) {
  normalizeAndExtractPolarization<GRID_SIZE, NUM_POL>(n, dst, src);
}
