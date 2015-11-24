#include <complex>
typedef std::complex<double> complexd;
#include "cfg.h"

struct visData {
  double u, v, w;
  complexd amp;
};

extern "C"
void gridKernel_scatter_full(
    double scale
  , complexd grid[]
  , const complexd gcf[OVER][OVER][GCF_SIZE][GCF_SIZE]
  , const visData data[num_baselines][num_times]
  , int grid_size
  );
