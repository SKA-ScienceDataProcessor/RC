#include "hogbom_cpu.h"
#include "mkHalideBuf.h"

#include <cstdio>
#include <cmath>
#include <cstring>
#include <cassert>

#ifdef __TEST
#define __COMBINED
#endif

extern "C"
void deconvolve(
    unsigned int niters
  , double gain
  , double threshold
#ifndef __COMBINED
  , bool ismodel
#endif
  , buffer_t * psf_buf_p
  , buffer_t * res_buf_p
  , buffer_t * mod_buf_p
  ) {

  int psf_peakx, psf_peaky;
  buffer_t psf_peakx_buf, psf_peaky_buf;
  psf_peakx_buf = psf_peaky_buf = mkHalideBuf<int>(1);
  psf_peakx_buf.host = tohost(&psf_peakx);
  psf_peaky_buf.host = tohost(&psf_peaky);

  double peakval;
  buffer_t peakval_buf;
  peakval_buf = mkHalideBuf<double>(1);
  peakval_buf.host = tohost(&peakval);

  // In fact we throw away peakval here
  find_peak_cpu(psf_buf_p, &psf_peakx_buf, &psf_peaky_buf, &peakval_buf);

#ifdef __COMBINED
  memset(mod_buf_p->host, 0, mod_buf_p->stride[1] * mod_buf_p->extent[1] * mod_buf_p->elem_size);

  for (unsigned int i = 0; i < niters; ++i) {
    resmodel_cpu(res_buf_p, psf_buf_p, gain, psf_peakx, psf_peaky, res_buf_p, mod_buf_p, &peakval_buf);
    if (fabs(peakval) < threshold) break;
  }
#else
  int (*kernel)(buffer_t *, buffer_t *, const double, const int32_t, const int32_t, buffer_t *, buffer_t *);
  if (ismodel) {
    memset(mod_buf_p->host, 0, mod_buf_p->stride[1] * mod_buf_p->extent[1] * mod_buf_p->elem_size);
    kernel = model_cpu;
  }
  else {
    assert(res_buf_p == mod_buf_p);
    kernel = res_cpu;
  }

  for (unsigned int i = 0; i < niters; ++i) {
    kernel(res_buf_p, psf_buf_p, gain, psf_peakx, psf_peaky, mod_buf_p, &peakval_buf);
    if (fabs(peakval) < threshold) break;
  }
#endif
}


#ifdef __TEST
#define __FMT "%6.1f"

const int siz = 20, pitch = 20;

double res[siz][pitch];
double psf[siz][pitch];
double mod[siz][pitch];

int main(){
  for (int x=0; x<20; x++) for (int y=0; y<20; y++) {
    res[x][y] = 300 - (x-10)*(x-10) - (y-10)*(y-10);
    // Transposed comparing to native Halide
    psf[x][y] = 150 - (x-11)*(x-11) - (y- 9)*(y- 9);
  }

  // All outputs below are transposed relatively to native Halide output

  printf("Res:\n");
  for(int i=0; i<20; i++) {
    for(int j=0; j<20; j++) printf(__FMT " ", res[i][j]);
    printf("\n");
  }
  printf("\nPSF:\n");
  for(int i=0; i<20; i++) {
    for(int j=0; j<20; j++) printf(__FMT " ", psf[i][j]);
    printf("\n");
  }

  buffer_t mod_buf = mkHalideBuf<double>(pitch, siz);
  mod_buf.host = tohost(mod);
  mod_buf.extent[1] = siz; // correct

  buffer_t res_buf = mkHalideBuf<double>(pitch, siz);
  res_buf.host = tohost(res);
  res_buf.extent[1] = siz; // correct

  buffer_t psf_buf = mkHalideBuf<double>(pitch, siz);
  psf_buf.host = tohost(psf);
  psf_buf.extent[1] = siz; // correct

  deconvolve(10, 0.01, 10.0, &psf_buf, &res_buf, &mod_buf);

  printf("\nResidual ...\n");
  for(int i=0; i<20; i++) {
    for(int j=0; j<20; j++) printf(__FMT " ", res[i][j]);
    printf("\n");
  }
  printf("\nModel ...\n");
  for(int i=0; i<20; i++) {
    for(int j=0; j<20; j++) printf(__FMT " ", mod[i][j]);
    printf("\n");
  }

}

#endif
