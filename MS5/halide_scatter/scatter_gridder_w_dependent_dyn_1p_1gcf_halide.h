#include "common.h"
#include "stats_n_utils.h"

void gridKernel_scatter_halide(
    double scale
  , double wstep
  , int baselines
  , const BlWMap bl_wp_map[/* baselines */]
  , const int bl_supps[/* baselines */] // computed from previous
  , complexd grids[]
    // w-planes length array of pointers
    // to [over][over][gcf_supp][gcf_supp] arrays
  , const complexd * gcf[]
  , const Double3 * _uvw[]
  , const complexd * _vis[]
  , int ts_ch
  , int grid_pitch
  , int grid_size
  );
