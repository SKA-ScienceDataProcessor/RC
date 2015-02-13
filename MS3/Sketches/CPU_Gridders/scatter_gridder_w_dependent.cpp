#if defined __AVX__
#include <immintrin.h>
#endif

#include <complex>

#ifdef __CUDACC__
typedef cuDoubleComplex complexd;
#else
typedef std::complex<double> complexd;
#endif

struct Double4c
{
  complexd XX;
  complexd XY;
  complexd YX;
  complexd YY;
};

struct Double3
{
  double u;
  double v;
  double w;
};

struct Pregridded
{
  short u;
  short v;
  short gcf_layer_index;
  short half_supp_size;
};


// We have original u,v,w, in meters.
// To go to u,v,w in wavelengths we shall multiply them with freq/SPEED_OF_LIGHT

#ifndef SPEED_OF_LIGHT
#define SPEED_OF_LIGHT 299792458.0
#endif

#ifndef WSTEP_CORRECT
#define WSTEP_CORRECT 0.00001
#endif

struct TaskCfg {
  double
      min_wave_length
    , max_inverse_wave_length
    , cellsize
    , cellsizeWL
    , scale
    , scaleWL
    , w_step
    , w_stepWL
    , w_shift
    , w_shiftWL
    ;
};

template <
    int w_planes
  , int half_supp_step
  , int grid_size
  , int over
  >
TaskCfg mkCfg (
    double min_u
  , double max_u
  , double min_v
  , double max_v
  , double min_w
  , double max_w
  , double max_freq
  ) {
  const int max_supp = (w_planes - 1) * half_supp_step * 2;
  const int uv_shift_in_pixels = (max_supp + grid_size) / 2;
  double
      min_wavelength = SPEED_OF_LIGHT / max_freq
    , max_inverse_wave_length = max_freq / SPEED_OF_LIGHT
    , maxx_u = std::max(max_u, -min_u)
    , maxx_v = std::max(max_v, -min_v)
    , maxx = std::max(maxx_u, maxx_v)
    , cellsize = maxx / double (uv_shift_in_pixels)
    , cellsizeWL = cellsize * max_inverse_wave_length
    , scale = double (uv_shift_in_pixels) / maxx
    , scaleWL = scale * min_wavelength
    , w_step = (max_w - min_w) / double(w_planes) + WSTEP_CORRECT
    , w_stepWL = w_step * scaleWL
    , w_shift = -min_w
    , w_shiftWL = w_shift * scaleWL
    ;
  return {
      min_wavelength
    , max_inverse_wave_length
    , cellsize
    , cellsizeWL
    , scale
    , scaleWL
    , w_step
    , w_stepWL
    , w_shift
    , w_shiftWL
    };
}

template <
    int w_planes
  , int half_supp_step
  , int grid_size
  , int over
  >
Pregridded pregrid(const Double3 & uvw, const TaskCfg & cfg) {
  const int max_supp = (w_planes - 1) * half_supp_step * 2;
  double
      us = uvw.u * cfg.scaleWL
    , vs = uvw.v * cfg.scaleWL
    ;
  int
      u = int(us)
    , v = int(vs)
    , wplane = int((uvw.w + cfg.w_shift)/ cfg.w_step)
    , uv_shift_in_pixels = (max_supp + grid_size) / 2
    ;
  short
      fracu = short(double(over) * (us - double(u)))
    , fracv = short(double(over) * (vs - double(v)))
    ;
  const int overu = over;
  const int overv = over;
  return {u + uv_shift_in_pixels, v + uv_shift_in_pixels, (wplane * overu + fracu) * overv + fracv, wplane * half_supp_step};
}

template <
    int w_planes
  , int half_supp_step
  , int grid_size
  , int over

  , int baselines
  , int timesteps
  , int channels
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
        max_supp_here = uvw[i].half_supp_size * 2;

      if (sv > max_supp_here)
        return; // CHECK_SUPP

      const complexd * gcf_layer = gcf + uvw[i].gcf_layer_index;

#ifdef __AVX__
      // We port Romein CPU code to doubles here (for MS2 we didn't)
      // vis0 covers XX and XY, vis1 -- YX and YY
      __m256d vis0 = _mm256_load_pd((const double *) &vis[i].XX);
      __m256d vis1 = _mm256_load_pd((const double *) &vis[i].YX);
#endif

      for (int su = 0; su <= max_supp_here; su++) {
      // NOTE: Romein writes about moving this to the very outer scope
      // (2 levels up) to make things more cache-friendly.
      // I don't know whether it is cache frendly
      // but what I definitely see gcc generates 4-times shorter code for it!
      // We need to investigate this!
      // for (int sv = 0; sv < max_supp_here; sv++) {
        int gsu, gsv;
        gsu = uvw[i].u + su - uvw[i].half_supp_size;
        gsv = uvw[i].v + sv - uvw[i].half_supp_size;

        int gcf_index = su * max_supp_here + sv;

#ifdef __AVX__
        __m256d weight_r = _mm256_set1_pd(gcf_layer[gcf_index].real());
        __m256d weight_i = _mm256_set1_pd(gcf_layer[gcf_index].imag());

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
        #define __GRID_POL(pol) grid[gsu][gsv].pol += vis[i].pol * gcf_layer[gcf_index]
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
template void gridKernel_scatter<32, 4, 2048, 8, 50*99, 20, 1>(
    const complexd * gcf
  , Double4c grid[2048][2048]
  , const Pregridded uvw[(50*90) * 20 * 1]
  , const Double4c vis[(50*90) * 20 * 1]
  );
