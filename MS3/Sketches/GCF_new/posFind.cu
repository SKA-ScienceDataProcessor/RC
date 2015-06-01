#include "threadFenceReduction_kernel.cuh"

struct place {
  __device__ place & operator=(const place & p) volatile {
      this->pos = p.pos;
      this->val = p.val;
      return const_cast<place&>(*this);
  }
  __device__ place (volatile place & p) : pos(p.pos), val(p.val){;}
  __device__ place (unsigned p, double v) : pos(p), val(v){;}
  
  size_t pos;
  double val;
};

struct posFinder {
  TASKCFG place init() {return {0u, 0.0};}
  TASKCFG place reduce(place x, place y){if (y.val > x.val) return y; else return x;}
  TASKCFG place f(unsigned int i, double c){return {i, fabs(c)};}
};

extern "C" __global__ void findPeak_512_e2(const double *g_idata, place *g_odata, unsigned int n) {
  reduceSinglePass_devGen<512, true, place, double, posFinder>(g_idata, g_odata, n);
}
