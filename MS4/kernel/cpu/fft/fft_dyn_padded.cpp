#ifdef _OPENMP
#include <omp.h>
#endif
#ifdef INTEL_MKL_VERSION
#include <fftw3_mkl.h>
#endif

#include "fft_dyn_padded.h"

template <bool oddSize, typename T>
inline void fft_center(T * data, int size, int pitch){
  bool flip = false;
  for (int i = 0; i < size * pitch; i+=pitch) {
    for (int j = 0; j < size; j++) {
      if (flip) data[i+j] = -data[i+j];
      flip = !flip;
    }
    if (oddSize) flip = !flip;
  }
}

// Very simple. Only for even sizes.
// And for even sizes fftshift and ifftshift coincide.
template <typename T>
void fftshift_even(T * data, int size, int pitch){
  T tmp;
  int
      halfsize = size / 2
    , midi = halfsize * pitch
    ;
  int i;
  for (i = 0; i < midi; i+=pitch) {
    for (int j = 0; j < halfsize; j++) {
       tmp = data[i+midi+j+halfsize];
       data[i+midi+j+halfsize] = data[i+j];
       data[i+j] = tmp;
    }
  }
  for (i = midi; i < size * pitch; i+=pitch) {
    for (int j = 0; j < halfsize; j++) {
       tmp = data[i-midi+j+halfsize];
       data[i-midi+j+halfsize] = data[i+j];
       data[i+j] = tmp;
    }
  }
}

template <int sign>
fftw_plan fft_plan_cc(
          int rank, const fftw_iodim *dims,
          int howmany_rank, const fftw_iodim *howmany_dims,
          fftw_complex *in, fftw_complex *out,
          unsigned flags){
  return fftw_plan_guru_dft(rank, dims, howmany_rank, howmany_dims, in, out, sign, flags);
}

template <int dir> struct plan_traits{};
#define __plan_trait(dir, fun, sta, dta, st, dt) \
template <> struct plan_traits<dir>{    \
  static fftw_plan_s* fft_plan(int a, const fftw_iodim *b, int c, const fftw_iodim* d, sta *e, dta *f, unsigned int g) { \
      return fun(a, b, c, d, e, f, g);  \
  }                                     \
  typedef st srcTy;                     \
  typedef dt dstTy;                     \
}

__plan_trait(-1, fft_plan_cc<-1>       , fftw_complex, fftw_complex, complexd, complexd);
__plan_trait( 0, fftw_plan_guru_dft_r2c, double,       fftw_complex, double  , complexd);
__plan_trait( 1, fft_plan_cc<1>        , fftw_complex, fftw_complex, complexd, complexd);
__plan_trait( 2, fftw_plan_guru_dft_c2r, fftw_complex, double,       complexd,   double);

inline fftw_complex * fftw_cast(complexd * const & p){return reinterpret_cast<fftw_complex *>(p);}
inline double * fftw_cast(double * const & p){return p;}

template <int dir>
fftw_plan __fft_inplace_even(fftw_plan p, void * data, int size, int pitch){
  // This does not quite work. Don't understand why yet.
  // fft_center<false>(data, size, pitch);
  #define src reinterpret_cast<typename plan_traits<dir>::srcTy *>(data)
  #define dst reinterpret_cast<typename plan_traits<dir>::dstTy *>(data)

  fftshift_even(src, size, pitch);

  if (p == NULL) {
    fftw_iodim trans_dims[2] = {
        {size, pitch, pitch}
      , {size, 1, 1}
      };
    p = plan_traits<dir>::fft_plan(
        2, trans_dims
      , 0, NULL
      , fftw_cast(src)
      , fftw_cast(dst)
      , FFTW_ESTIMATE
      );
  }
  fftw_execute(p);

  fftshift_even(dst, size, pitch);

  return p;
}

fftw_plan fft_inplace_even(fftw_plan p, int sign, void * data, int size, int pitch){
  #define __sw(d) case d: __fft_inplace_even<d>(p, data, size, pitch); return p;
  switch(sign){
    __sw(-1);
    __sw( 0);
    __sw( 1);
    __sw( 2);
    default: return nullptr;
  }
}

void fftInitThreading() {
#ifdef _OPENMP
#ifdef INTEL_MKL_VERSION
// NOTE: Using Intel MKL (and threading particularly)
//   could require a lot of additional setup
  fftw3_mkl.number_of_user_threads = omp_get_max_threads();
#else
  fftw_init_threads();
  fftw_plan_with_nthreads(omp_get_max_threads());
#endif
#endif
}
