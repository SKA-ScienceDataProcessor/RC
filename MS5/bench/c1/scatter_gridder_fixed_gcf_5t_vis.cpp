#include <vector>
#include <cstring>
#include <ctime>

#if defined _OPENMP
#include <omp.h>
#else
#define omp_get_num_threads() 1
#define omp_get_thread_num()  0
#endif

#if defined __AVX__
#include <immintrin.h>
#define __MMSIZE 32
typedef __m256d __mdType;
#define asMdp(p) (reinterpret_cast<__m256d*>(p))
#define asMdpc(p) (reinterpret_cast<const __m256d*>(p))
#define _mm_add_pd _mm256_add_pd
#else
#include <smmintrin.h>
#define __MMSIZE 16
typedef __m128d __mdType;
#define asMdp(p) (reinterpret_cast<__m128d*>(p))
#define asMdpc(p) (reinterpret_cast<const __m128d*>(p))
#endif

#include "aligned_malloc.h"

#include "scatter_gridder_fixed_gcf_5t_vis.h"

const int over = OVER;

inline
void addGrids(
    complexd dst[]
  , const complexd srcs[]
  , int nthreads
  , int grid_size
  )
{
  int siz = grid_size*grid_size;
#pragma omp parallel for
  for (int i = 0; i < int(siz*sizeof(complexd)/__MMSIZE); i++) {
    __mdType sum = asMdpc(srcs)[i];
    // __m256d sum = _mm256_loadu_pd(reinterpret_cast<const double*>(as256pc(srcs)+i));

    for (int g = 1; g < nthreads; g ++)
      sum = _mm_add_pd(sum, asMdpc(srcs + g * siz)[i]);

    asMdp(dst)[i] = sum;
  }
}

struct pre {
  short u, v, overu, overv;
};

inline
pre prep(int grid_size, double scale, const visData & vd){
  double
      us = vd.u * scale
    , vs = vd.v * scale
    ;
  short
      u = short(floor(us))
    , v = short(floor(vs))
    , overu = short(floor(over * (us - u)))
    , overv = short(floor(over * (vs - v)))
    ;
  u += short(grid_size / 2 - GCF_SIZE / 2);
  v += short(grid_size / 2 - GCF_SIZE / 2);
  return {u, v, overu, overv};
}

void gridKernel_scatter(
    double scale
  , complexd grids[]
  , const complexd gcf[over][over][GCF_SIZE][GCF_SIZE]
  , const visData data[num_baselines][num_times]
  , int grid_size
  ) {
#pragma omp parallel
  {
    int siz = grid_size*grid_size;
    complexd * grid = grids + omp_get_thread_num() * siz;
    // Moved up the call stack to make benchmarks cleaner
    // memset(grid, 0, sizeof(complexd) * siz);

#pragma omp for schedule(dynamic)
    for(int bl = 0; bl < num_baselines; bl++) {
      typedef const complexd gcfl[GCF_SIZE][GCF_SIZE];
      gcfl * gcflp[num_times];
      pre pa[num_times];

      for(int n=0; n < num_times; n++){
        pa[n] = prep(grid_size, scale, data[bl][n]);
        gcflp[n] = &gcf[pa[n].overu][pa[n].overv];
      }
      for (int su = 0; su < GCF_SIZE; su++) { // Moved from 2-levels below according to Romein
        for (int i = 0; i < num_times; i++) {
          const complexd * gcfp;
          gcfp = (*gcflp[i])[su];
          for (int sv = 0; sv < GCF_SIZE; sv++) {
            // Don't forget our u v are already translated by -max_supp_here/2
            int gsu, gsv;
            gsu = pa[i].u + su;
            gsv = pa[i].v + sv;
            if (gsu < 0 || gsu >= grid_size || gsv < 0 || gsv >= grid_size) continue;
            grid[gsu * grid_size + gsv] += data[bl][i].amp * gcfp[sv];
          }
        }
      }
    }
  }
}

// We benchmark different parts separately
void gridKernel_scatter_full(
    double scale
  , complexd grid[]
  , const complexd gcf[over][over][GCF_SIZE][GCF_SIZE]
  , const visData data[num_baselines][num_times]
  , int grid_size
  ) {
  int siz = grid_size*grid_size;
  int nthreads;
  clock_t ti, ti1;
#ifdef _OPENMP
#pragma omp parallel
#pragma omp single
  nthreads = omp_get_num_threads();
  complexd * tmpgrids = alignedMallocArray<complexd>(siz * nthreads, 32);
  memset(grid, 0, sizeof(complexd) * siz);
  // Moved here from
  memset(tmpgrids, 0, sizeof(complexd) * nthreads * siz);
  ti = clock();
  gridKernel_scatter(scale, tmpgrids, gcf, data, grid_size);
  ti1 = clock();
  printf("%ld+", ti1 - ti);
  addGrids(grid, tmpgrids, nthreads, grid_size);
  printf("%ld\n", clock() - ti1);
  _aligned_free(tmpgrids);
#else
  ti = clock();
  gridKernel_scatter(scale, grid, gcf, data, grid_size);
  printf("%ld\n", clock() - ti);
#endif
}
