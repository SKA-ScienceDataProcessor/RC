#include "threadFenceReduction_kernel.cuh"

#include <math_functions.h>

struct doubleMax {
  TASKCFG double init() {return 0.0;}
  TASKCFG double reduce(double x, double acc){return max(x,acc);}
  TASKCFG double f(unsigned int, double c){return fabs(c);}
};

extern "C" __global__ void max_512_e2(const double *g_idata, double *g_odata, unsigned int n) {
  reduceSinglePass_devGen<512, true, double, double, doubleMax>(g_idata, g_odata, n);
}
