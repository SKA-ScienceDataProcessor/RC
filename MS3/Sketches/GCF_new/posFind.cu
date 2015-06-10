#include "threadFenceReduction_kernel.cuh"
#include "posFind.cuh"

struct posFinder {
  TASKCFG place init() {return place(0u, 0.0);}
  TASKCFG place reduce(place x, place y){if (y.val > x.val) return y; else return x;}
  TASKCFG place f(unsigned int i, double c){return place(i, fabs(c));}
};

__global__ void findPeak_512_e2(const double *g_idata, place *g_odata, unsigned int n) {
  reduceSinglePass_devGen<512, true, place, double, posFinder>(g_idata, g_odata, n);
}
