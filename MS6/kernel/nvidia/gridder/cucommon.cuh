#ifndef __CUCOMMON_CUH
#define __CUCOMMON_CUH

#include <cuda.h>

void CUDA_CHECK_ERR(unsigned lineNumber, const char* fileName);
float getElapsed(cudaEvent_t start, cudaEvent_t stop);

#endif
