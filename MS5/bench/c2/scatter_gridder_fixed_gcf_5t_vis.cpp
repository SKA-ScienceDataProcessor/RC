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

struct preover {
  short overu, overv;
};

struct preuv {
  short u, v;
};

inline void
prep(int grid_size, double scale, const visData & vd, preover & po, preuv & puv){
  double
      us = vd.u * scale
    , vs = vd.v * scale
    ;
  short
      u = short(floor(us))
    , v = short(floor(vs))
    ;
  po.overu = short(floor(over * (us - u)));
  po.overv = short(floor(over * (vs - v)));
  puv.u = u + short(grid_size / 2 - GCF_SIZE / 2);
  puv.v = v + short(grid_size / 2 - GCF_SIZE / 2);
}

void gridKernel_scatter(
    double scale
  , complexd grids[]
  , const complexd gcf[over][over][GCF_SIZE][GCF_SIZE]
  , const visData data[num_baselines*num_times]
  , int grid_size
  ) {
#pragma omp parallel
  {
    int siz = grid_size*grid_size;
    complexd * grid = grids + omp_get_thread_num() * siz;
    // MOVED !!! memset(grid, 0, sizeof(complexd) * siz);

    const int tms = num_baselines*num_times;
    typedef const complexd (*gcfl)[GCF_SIZE][GCF_SIZE];
    gcfl * gcflp = new gcfl[tms];
    preover * pov = new preover[tms];
    preuv * puv = new preuv[tms];
    #pragma omp for schedule(dynamic)
    for(int n = 0; n < tms; n++) {
      prep(grid_size, scale, data[n], pov[n], puv[n]);
      gcflp[n] = &gcf[pov[n].overu][pov[n].overv];
    }
    #pragma omp for schedule(dynamic)
    for (int su = 0; su < GCF_SIZE; su++) { // Moved from 2-levels below according to Romein
      for (int i = 0; i < tms; i++) {
        const complexd * gcfp;
        gcfp = (*gcflp[i])[su];
        for (int sv = 0; sv < GCF_SIZE; sv++) {
          // Don't forget our u v are already translated by -max_supp_here/2
          int gsu, gsv;
          gsu = puv[i].u + su;
          gsv = puv[i].v + sv;
          if (gsu < 0 || gsu >= grid_size || gsv < 0 || gsv >= grid_size) continue;
          grid[gsu * grid_size + gsv] += data[i].amp * gcfp[sv];
        }
      }
    }
    delete [] puv;
    delete [] pov;
    delete [] gcflp;
  }
}

void gridKernel_scatter_full(
    double scale
  , complexd grid[]
  , const complexd gcf[over][over][GCF_SIZE][GCF_SIZE]
  , const visData data[num_baselines * num_times]
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
  memset(grid, 0, sizeof(complexd) * siz); // MOVED !!!
  memset(tmpgrids, 0, sizeof(complexd) * nthreads * siz); // MOVED !!!
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
