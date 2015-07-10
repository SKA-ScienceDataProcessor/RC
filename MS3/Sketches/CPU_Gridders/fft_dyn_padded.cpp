#include <fftw3.h>
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
void __fft_inplace(complexd * data, int size, int pitch){
  // This does not quite work. Don't understand why yet.
  // fft_center<false>(data, size, pitch);
  fftshift_even(data, size, pitch);

  fftw_iodim trans_dims[2] = {
      {size, pitch, pitch}
    , {size, 1, 1}
    };
  fftw_plan p = fftw_plan_guru_dft(
      2, trans_dims
    , 0, NULL
    , dp, dp
    , FFTW_FORWARD, FFTW_ESTIMATE
    );
  fftw_execute(p);
  fftw_destroy_plan(p);

  fftshift_even(data, size, pitch);
}

extern "C"
void fft_inplace_even(complexd * data, int size, int pitch){
  __fft_inplace(data, size, pitch);
}
