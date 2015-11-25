#include <vector>
#include <cstring>
#include <ctime>

#include <cstdio>

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

#ifndef _WIN32
#define TTF "%ld"
static timespec ts;
void clock_start(){
  clock_gettime(CLOCK_REALTIME, &ts);
}
time_t clock_diff() {
  timespec ts2;
  clock_gettime(CLOCK_REALTIME, &ts2);
  return (time_t)(ts2.tv_sec - ts.tv_sec) * 1000000000 +
         (time_t)(ts2.tv_nsec - ts.tv_nsec);
}
#else
#define TTF "%lld"
static time_t t;
void clock_start(){t = clock();}
time_t clock_diff() {return clock() - t;}
#endif

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

template <int gcf_size>
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
  u += short(grid_size / 2 - gcf_size / 2);
  v += short(grid_size / 2 - gcf_size / 2);
  return {u, v, overu, overv};
}

template <int gcf_size>
void gridKernel_scatter_chunked(
    double scale
  , complexd grids[]
  , const complexd gcf[over][over][gcf_size][gcf_size]
  , const visData data[num_baselines][num_times]
  , int grid_size
  ) {
#pragma omp parallel
  {
    int siz = grid_size*grid_size;
    complexd * grid = grids + omp_get_thread_num() * siz;

#pragma omp for schedule(dynamic)
    for(int bl = 0; bl < num_baselines; bl++) {
      typedef const complexd gcfl[gcf_size][gcf_size];
      gcfl * gcflp[num_times];
      pre pa[num_times];
      bool inbound[num_times];

      for(int n=0; n < num_times; n++){
        pa[n] = prep<gcf_size>(grid_size, scale, data[bl][n]);
        gcflp[n] = &gcf[pa[n].overu][pa[n].overv];
        inbound[n] = pa[n].u >= 0 && pa[n].u < grid_size - gcf_size
                  && pa[n].v >= 0 && pa[n].v < grid_size - gcf_size;
      }
      for (int su = 0; su < gcf_size; su++) { // Moved from 2-levels below according to Romein
        for (int i = 0; i < num_times; i++) {
          const complexd * gcfp;
          gcfp = (*gcflp[i])[su];
          for (int sv = 0; sv < gcf_size; sv++) {
            // Don't forget our u v are already translated by -max_supp_here/2
            if (inbound[i])
              grid[(pa[i].u + su) * grid_size + (pa[i].v + sv)] += data[bl][i].amp * gcfp[sv];
          }
        }
      }
    }
  }
}

struct preover {
  short overu, overv;
};

struct preuv {
  short u, v;
};

template <int gcf_size>
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
  puv.u = u + short(grid_size / 2 - gcf_size / 2);
  puv.v = v + short(grid_size / 2 - gcf_size / 2);
}

template <int gcf_size>
void gridKernel_scatter(
    double scale
  , complexd grids[]
  , const complexd gcf[over][over][gcf_size][gcf_size]
  , const visData data[num_baselines*num_times]
  , int grid_size
  ) {
#pragma omp parallel
  {
    int siz = grid_size*grid_size;
    complexd * grid = grids + omp_get_thread_num() * siz;

    const int tms = num_baselines*num_times;
    typedef const complexd (*gcfl)[gcf_size][gcf_size];
    gcfl * gcflp = new gcfl[tms];
    preover * pov = new preover[tms];
    preuv * puv = new preuv[tms];
    bool * inbound = new bool[tms];
    #pragma omp for schedule(dynamic)
    for(int n = 0; n < tms; n++) {
      prep<gcf_size>(grid_size, scale, data[n], pov[n], puv[n]);
      gcflp[n] = &gcf[pov[n].overu][pov[n].overv];
      inbound[n] = puv[n].u >= 0 && puv[n].u < grid_size - gcf_size
                && puv[n].v >= 0 && puv[n].v < grid_size - gcf_size;
    }
    #pragma omp for schedule(dynamic)
    for (int su = 0; su < gcf_size; su++) { // Moved from 2-levels below according to Romein
      for (int i = 0; i < tms; i++) {
        const complexd * gcfp;
        gcfp = (*gcflp[i])[su];
        for (int sv = 0; sv < gcf_size; sv++) {
          // Don't forget our u v are already translated by -max_supp_here/2
          if (inbound[i])
            grid[(puv[i].u + su) * grid_size + (puv[i].v + sv)] += data[i].amp * gcfp[sv];
        }
      }
    }
    delete [] inbound;
    delete [] puv;
    delete [] pov;
    delete [] gcflp;
  }
}

// We benchmark different parts separately
template <bool chunked, int gcf_size, kern_type<chunked, gcf_size> kern>
void gridKernel_scatter_full(
    double scale
  , complexd grid[]
  , const complexd gcf[over][over][gcf_size][gcf_size]
  , typename dlayout<chunked>::type data
  , int grid_size
  ) {
  int siz = grid_size*grid_size;
  int nthreads;
#ifdef _OPENMP
#pragma omp parallel
#pragma omp single
  nthreads = omp_get_num_threads();
  complexd * tmpgrids = alignedMallocArray<complexd>(siz * nthreads, 32);
  memset(grid, 0, sizeof(complexd) * siz);
  // Moved here from the kernel
  clock_start();
#pragma omp parallel
  memset(tmpgrids + omp_get_thread_num() * siz, 0, sizeof(complexd) * siz);
  printf("gcf size %d: " TTF "+", gcf_size, clock_diff());
  clock_start();
  kern(scale, tmpgrids, gcf, data, grid_size);
  printf(TTF "+", clock_diff());
  clock_start();
  addGrids(grid, tmpgrids, nthreads, grid_size);
  printf(TTF "\n", clock_diff());
  _aligned_free(tmpgrids);
#else
  clock_start();
  kern(scale, grid, gcf, data, grid_size);
  printf(TTF "\n", clock_diff());
#endif
}

#define __KERNEL_CHUNKED_IMP(sz)                                                                        \
__KERNEL_CHUNKED_DECL(sz){                                                                              \
  gridKernel_scatter_full<true, sz, gridKernel_scatter_chunked<sz>>(scale, grid, gcf, data, grid_size); \
}

__KERNEL_CHUNKED_IMP(16)
__KERNEL_CHUNKED_IMP(32)

#define __KERNEL_IMP(sz)                                                                         \
__KERNEL_DECL(sz){                                                                               \
  gridKernel_scatter_full<false, sz, gridKernel_scatter<sz>>(scale, grid, gcf, data, grid_size); \
}

__KERNEL_IMP(16)
__KERNEL_IMP(32)
