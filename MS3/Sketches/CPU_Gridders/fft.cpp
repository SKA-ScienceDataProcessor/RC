#include <fftw3.h>
#include "common.h"
#include "metrix.h"

// Very simple. Only for even sizes.
// And for even sizes fftshift and ifftshift coinside.
template<int N> void fftshift_even(complexd data[N][N]){
  complexd tmp;
  for (int i = 0; i < N/2; i++) {
    for (int j = 0; j < N/2; j++) {
       tmp = data[i+N/2][j+N/2];
       data[i+N/2][j+N/2] = data[i][j];
       data[i][j] = tmp;
    }
  }
}

#define dp reinterpret_cast<fftw_complex*>(&data[0][0])
template<int N> void __fft_inplace(complexd data[N][N]){
  fftshift_even<N>(data);
  fftw_plan p = fftw_plan_dft_2d(N, N, dp, dp, FFTW_FORWARD, FFTW_ESTIMATE);
  fftw_execute(p);
  fftw_destroy_plan(p);
  fftshift_even<N>(data);
}

extern "C"
void fft_inplace_even(complexd data[GRID_SIZE][GRID_SIZE]){
  __fft_inplace<GRID_SIZE>(data);
}
