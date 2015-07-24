#ifndef __FFT_DYN_PADDED_H
#define __FFT_DYN_PADDED_H

#include <fftw3.h>
#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

fftw_plan fft_inplace_even(fftw_plan p, int sign, void * data, int size, int pitch);
void fftInitThreading();

#ifdef __cplusplus
}
#endif

#endif
