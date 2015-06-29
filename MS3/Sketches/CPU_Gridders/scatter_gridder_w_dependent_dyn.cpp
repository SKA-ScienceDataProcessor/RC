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

inline
void addGrids(
    Double4c dst[]
  , const Double4c srcs[]
  , int nthreads
  , int grid_pitch
  , int grid_size
  )
{
  int siz = grid_size*grid_pitch;
#pragma omp parallel for
  for (unsigned int i = 0; i < siz*sizeof(Double4c)/(256/8); i++) {
    __m256d sum = as256pc(srcs)[i];
    // __m256d sum = _mm256_loadu_pd(reinterpret_cast<const double*>(as256pc(srcs)+i));

    for (int g = 1; g < nthreads; g ++)
      sum = _mm256_add_pd(sum, as256pc(srcs + g * siz)[i]);

    as256p(dst)[i] = sum;
  }
}

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
  , Double4c grids[]
    // We have a [w_planes][over][over]-shaped array of pointers to
    // variable-sized gcf layers, but we precompute (in pregrid)
    // exact index into this array, thus we use plain pointer here
  , const complexd * gcf[]
  , const Inp _uvw[]
  , const Double4c _vis[]
  , int ts_ch
  , int grid_pitch
  , int grid_size
  ) {
  int siz = grid_size*grid_pitch;
#pragma omp parallel
  {
    Double4c * _grid = grids + omp_get_thread_num() * siz;
    memset(_grid, 0, sizeof(Double4c) * siz);
    __ACC(Double4c, grid, grid_pitch);

#pragma omp for schedule(dynamic)
    for(int bl0 = 0; bl0 < baselines; bl0++) {
      int bl;
      if (use_permutations) bl = permutations[bl0].bl;
      else bl = bl0;
      int max_supp_here;
      max_supp_here = get_supp(permutations[bl].wp);

      // Rotation
      // VLA requires "--std=gnu..." extension
      Double4c vis[ts_ch];
      int off;
      off = bl*ts_ch;
      const Inp * uvw;
      uvw = _uvw + off;
      for(int n=0; n<ts_ch; n++){
        #define __ROT_N_COPY(pol) vis[n].pol = rotw(_vis[off + n].pol, uvw[n].w);
        __ROT_N_COPY(XX)
        __ROT_N_COPY(XY)
        __ROT_N_COPY(YX)
        __ROT_N_COPY(YY)
      }
      
      for (int su = 0; su < max_supp_here; su++) { // Moved from 2-levels below according to Romein
        for (int i = 0; i < ts_ch; i++) {
            Pregridded p;
            cvt<over, is_half_gcf, Inp>::pre(scale, wstep, uvw[i], p, grid_size);

#ifdef __AVX__
          // We port Romein CPU code to doubles here (for MS2 we didn't)
          // vis0 covers XX and XY, vis1 -- YX and YY
          __m256d vis0, vis1;
          vis0 = _mm256_load_pd((const double *) &vis[i].XX);
          vis1 = _mm256_load_pd((const double *) &vis[i].YX);
          // vis0 = _mm256_loadu_pd((const double *) &vis[bl][i].XX);
          // vis1 = _mm256_loadu_pd((const double *) &vis[bl][i].YX);
#endif
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
            gridptr##p[p] = _mm256_add_pd(gridptr##p[p], t##p)
            //_mm256_storeu_pd(reinterpret_cast<double*>(gridptr##p + p), _mm256_add_pd(gridptr##p[p], t##p));

            __DO(0, XX);
            __DO(1, YX);
#else
            #define __GRID_POL(pol) grid[gsu][gsv].pol += vis[i].pol * supportPixel
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
  , Double4c grid[]
    // We have a [w_planes][over][over]-shaped array of pointers to
    // variable-sized gcf layers, but we precompute (in pregrid)
    // exact index into this array, thus we use plain pointer here
  , const complexd * gcf[]
  , const Inp uvw[]
  , const Double4c vis[]
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
  memset(grid, 0, sizeof(Double4c) * siz);
  Double4c * tmpgrids = alignedMallocArray<Double4c>(siz * nthreads, 32);
  
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
  , Double4c grid[]                                        \
  , const complexd * gcf[]                                 \
  , const Double3 uvw[]                                    \
  , const Double4c vis[]                                   \
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

typedef complexd poltyp[4];

extern "C"
void normalizeAndExtractPolarization(
    int n
  , complexd dst[]
  , const poltyp src[]
  , int grid_pitch
  , int grid_size
  )
{
  int siz = grid_size*grid_pitch;
  const double norm = 1.0/double(siz);
#pragma omp parallel for
  for (int i = 0; i < siz; i++) {
    dst[i] = src[i][n] * norm;
  }
}
