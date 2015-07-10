#include <fftw3.h>
#include "common.h"
#include "metrix.h"

template<int N> void fft_center(complexd data[N][N]){
  bool flip = false;
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      if (flip) data[i][j] = std::complex<double>(-data[i][j].real(), -data[i][j].imag());
      flip = !flip;
    }
  }
}

// Very simple. Only for even sizes.
// And for even sizes fftshift and ifftshift coinside.
template<int N> void fftshift_even(complexd data[N][N]){
  complexd tmp;
  int i;
  for (i = 0; i < N/2; i++) {
    for (int j = 0; j < N/2; j++) {
       tmp = data[i+N/2][j+N/2];
       data[i+N/2][j+N/2] = data[i][j];
       data[i][j] = tmp;
    }
  }
  for (i = N/2; i < N; i++) {
    for (int j = 0; j < N/2; j++) {
       tmp = data[i-N/2][j+N/2];
       data[i-N/2][j+N/2] = data[i][j];
       data[i][j] = tmp;
    }
  }
}

#define dp reinterpret_cast<fftw_complex*>(&data[0][0])
template<int N> void __fft_inplace(complexd data[N][N]){
  fft_center<N>(data);
  fftw_plan p = fftw_plan_dft_2d(N, N, dp, dp, FFTW_FORWARD, FFTW_ESTIMATE);
  fftw_execute(p);
  fftw_destroy_plan(p);
  // Temporarily disable the shifting of the result because
  // of slowness
  // fftshift_even<N>(data);
}

extern "C"
void fft_inplace_even(complexd data[GRID_SIZE][GRID_SIZE]){
  __fft_inplace<GRID_SIZE>(data);
}
