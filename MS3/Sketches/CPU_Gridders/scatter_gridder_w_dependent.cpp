#include "common.h"
#include "metrix.h"

template <
    int grid_size

  , int baselines
  , int timesteps
  , int channels
  , bool is_half_gcf
  >
// grid must be initialized to 0s.
void gridKernel_scatter(
    // We have a [w_planes][over][over]-shaped array of pointers to
    // variable-sized gcf layers, but we precompute (in pregrid)
    // exact index into this array, thus we use plain pointer here
    const complexd * gcf
  , Double4c grid[grid_size][grid_size]
  // , const Pregridded uvw[baselines][timesteps][channels]
  // , const Double4c vis[baselines][timesteps][channels]
  , const Pregridded uvw[baselines * timesteps * channels]
  , const Double4c vis[baselines * timesteps * channels]
  ) {
  for (int sv = 0; /* see CHECK_SUPP comment */ ; sv++) { // Moved from 2-levels below according to Romein
    for (int i = 0; i < baselines * timesteps * channels; i++) {
      const int
        max_supp_here = uvw[i].gcf_layer_supp;

      if (sv > max_supp_here)
        return; // CHECK_SUPP

#ifdef __AVX__
      // We port Romein CPU code to doubles here (for MS2 we didn't)
      // vis0 covers XX and XY, vis1 -- YX and YY
      __m256d vis0, vis1;
      vis0 = _mm256_load_pd((const double *) &vis[i].XX);
      vis1 = _mm256_load_pd((const double *) &vis[i].YX);
#endif

      for (int su = 0; su <= max_supp_here; su++) {
      // NOTE: Romein writes about moving this to the very outer scope
      // (2 levels up) to make things more cache-friendly.
      // I don't know whether it is cache frendly
      // but what I definitely see gcc generates 4-times shorter code for it!
      // We need to investigate this!
      // for (int sv = 0; sv < max_supp_here; sv++) {
        // Don't forget our u v are already translated by -max_supp_here/2
        int gsu, gsv;
        gsu = uvw[i].u + su;
        gsv = uvw[i].v + sv;

        complexd supportPixel;
        #define __layeroff su * max_supp_here + sv
        if (is_half_gcf) {
          int index;
          index = uvw[i].gcf_layer_index;
          // Negative index indicates that original w was mirrored
          // and we shall negate the index to obtain correct
          // offset *and* conjugate the result.
          if (index < 0) {
            supportPixel = conj((gcf - index)[__layeroff]);
          } else {
            supportPixel = (gcf + index)[__layeroff];
          }
        } else {
            supportPixel = (gcf + uvw[i].gcf_layer_index)[__layeroff];
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


// Test instantiation
void gridKernel_scatter1(
    const complexd * gcf
  , Double4c grid[GRID_SIZE][GRID_SIZE]
  , const Pregridded uvw[BASELINES * TIMESTEPS * CHANNELS]
  , const Double4c vis[BASELINES * TIMESTEPS * CHANNELS]
  ){
  gridKernel_scatter<GRID_SIZE, BASELINES, TIMESTEPS, CHANNELS, false>(gcf, grid, uvw, vis);
  }
