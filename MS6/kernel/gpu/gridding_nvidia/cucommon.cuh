#ifndef __CUCOMMON_CUH
#define __CUCOMMON_CUH

#ifndef __clang__
#include <cuda.h>
#else
#include "C:\Progs\clang-HEAD-x86_64-w64-windows-gnu-s\lib\clang\3.9.0\include\__clang_cuda_runtime_wrapper.h"
#endif

void CUDA_CHECK_ERR(unsigned lineNumber, const char* fileName);
float getElapsed(cudaEvent_t start, cudaEvent_t stop);

#endif
