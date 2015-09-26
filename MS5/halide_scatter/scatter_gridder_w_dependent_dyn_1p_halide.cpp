#ifdef _MSC_VER
#include <vector>
#endif

#include <cstring>
#include <complex>
#include <tuple>

#include <omp.h>

#include "uvgsh.h"
#include "mkHalideBuf.h"

using namespace std;

typedef complex<double> complexd;

const int over = 8;
void gridKernel_scatter_halide(
    double scale
  , double wstep
  , int baselines
  , const int bl_supps[/* baselines */]
  , complexd grids[]
    // GCF *data* ptr
    // packed [over][over][gcf_supp][gcf_supp] arrays
    // we have w-planes of them. gcf varies between w-plane values.
    // Halide gridder computes all offsets into it
  , const complexd gcf[]
  , const Double3 * _uvw[]
  , const complexd * _vis[]
  , int ts_ch
  , int grid_pitch
  , int grid_size
  , int gcf_supps[]
  // Provisional.
  // Needed only to compute gcf layers offsets.
  , int wplanes
  ) {
  #ifndef _MSC_VER
    int gcf_offs_[wplanes];
    #define gcf_offs gcf_offs_
  #else
    vector<int> gcf_offs_(wplanes);
    #define gcf_offs gcf_offs_.data()
  #endif
  int off = 0;
  for(int wp=0; wp<wplanes; wp++){
    gcf_offs_[wp] = off;
    off += over * over * gcf_supps[wp] * gcf_supps[wp];
  }
  buffer_t gcfsup_buf = mk1DHalideBuf(gcf_supps, wplanes);
  buffer_t gcfoff_buf = mk1DHalideBuf(gcf_offs, wplanes);
  tuple<buffer_t, buffer_t> gcf_buf = mk1DHalideBuf(gcf, off);
  buffer_t uvw_buf = mk2DHalideBuf<Double3>(ts_ch);
  buffer_t vis_buf = mk2DHalideBuf<complexd>(ts_ch);

#pragma omp parallel
  {
    int siz = grid_size*grid_pitch;
    complexd * grid = grids + omp_get_thread_num() * siz;

    memset(grid, 0, sizeof(complexd) * siz);
    tuple<buffer_t, buffer_t> grid_buf = mk1DHalideBuf(grid, siz);

#pragma omp for schedule(dynamic,23)
    for(int bl = 0; bl < baselines; bl++){
      set2DHalideBuf(_uvw[bl], uvw_buf);
      set2DHalideBuf(_vis[bl], vis_buf);
      uvgsh(
          scale
        , wstep
        , bl_supps[bl]
        , ts_ch
        , grid_pitch
        , grid_size
        , &gcfsup_buf
        , &uvw_buf
        , &vis_buf
        , off
        , &gcfoff_buf
        , &get<0>(gcf_buf)
        , &get<1>(gcf_buf)
        , &get<0>(grid_buf)
        , &get<1>(grid_buf)
        );
    }
  }
}
