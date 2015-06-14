struct place {
  __host__ __device__ place & operator=(const place & p) volatile {
      this->pos = p.pos;
      this->val = p.val;
      return const_cast<place&>(*this);
  }
  __host__ __device__ place () {;}
  __host__ __device__ place (unsigned p, double v) : pos(p), val(v){;}
  __host__ __device__ place (const place & p) : pos(p.pos), val(p.val){;}
  __host__ __device__ place (const volatile place & p) : pos(p.pos), val(p.val){;}
  
  size_t pos;
  double val;
};

extern "C" __global__ void findPeak_512_e2(const double *g_idata, place *g_odata, unsigned int n);
