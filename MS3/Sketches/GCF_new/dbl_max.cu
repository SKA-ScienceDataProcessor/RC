#include "threadFenceReduction_kernel.cuh"

struct doubleMax {
  TASKCFG double init() {return 0.0;}
  TASKCFG double reduce(double x, double acc){return x > acc ? x : acc;}
  TASKCFG double f(double c){return fabs(c);}
};

extern "C" __global__ void max_512_e2(const double *g_idata, double *g_odata, unsigned int n) {
  retirementCount = 0;
  reduceSinglePass_devGen<512, true, double, double, doubleMax>(g_idata, g_odata, n);
}
