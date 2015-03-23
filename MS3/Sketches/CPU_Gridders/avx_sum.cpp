#if defined __AVX__

#include <immintrin.h>
#include <complex>
typedef std::complex<double> complexd;

#define as256p(p) (reinterpret_cast<__m256d*>(p))

// n should be even
extern "C"
void addComplexVectors(complexd * dst, complexd * src, int n) {
  for(int i = 0; i < n/2; i++)
    as256p(dst)[i] = _mm256_add_pd(as256p(dst)[i], as256p(src)[i]);
}

#endif
