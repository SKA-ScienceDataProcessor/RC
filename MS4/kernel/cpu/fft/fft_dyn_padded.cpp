#ifdef _OPENMP
#include <omp.h>
#endif
#include <fftw3.h>
#ifdef INTEL_MKL_VERSION
#include "fftw3_mkl.h"
#endif

#include "common.h"

template <bool oddSize>
inline void fft_center(complexd * data, int size, int pitch){
  bool flip = false;
  for (int i = 0; i < size * pitch; i+=pitch) {
    for (int j = 0; j < size; j++) {
      if (flip) data[i+j] = complexd(-data[i+j].real(), -data[i+j].imag());
      flip = !flip;
    }
    if (oddSize) flip = !flip;
  }
}

// Very simple. Only for even sizes.
// And for even sizes fftshift and ifftshift coinside.
inline void fftshift_even(complexd * data, int size, int pitch){
  complexd tmp;
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

#define dp reinterpret_cast<fftw_complex*>(data)
fftw_plan __fft_inplace(fftw_plan p, complexd * data, int size, int pitch){
  // This does not quite work. Don't understand why yet.
  // fft_center<false>(data, size, pitch);
  fftshift_even(data, size, pitch);

  if (p == NULL) {
    fftw_iodim trans_dims[2] = {
        {size, pitch, pitch}
      , {size, 1, 1}
      };
    p = fftw_plan_guru_dft(
        2, trans_dims
      , 0, NULL
      , dp, dp
      , FFTW_FORWARD, FFTW_ESTIMATE
      );
  }
  fftw_execute(p);

  fftshift_even(data, size, pitch);

  return p;
}

extern "C" {

fftw_plan fft_inplace_even(fftw_plan p, complexd * data, int size, int pitch){
  return __fft_inplace(p, data, size, pitch);
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

}