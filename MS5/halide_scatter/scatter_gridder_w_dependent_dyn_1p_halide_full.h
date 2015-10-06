#include "common.h"

void gridKernel_scatter_halide_full(
    double scale
  , double wstep
  , int baselines
  , const int bl_supps[/* baselines */]
  , complexd grid[]
    // GCF *data* ptr
    // packed [over][over][gcf_supp][gcf_supp] arrays
    // we have w-planes of them. gcf varies between w-plane values.
    // Halide gridder computes all offsets into it
  , const complexd gcf[]
  , const Double3 uvw[]
  , const complexd vis[]
  , int ts_ch
  , int grid_pitch
  , int grid_size
  , int gcf_supps[]
  // Provisional.
  // Needed only to compute gcf layers offsets.
  , int maxWPlane
  );
