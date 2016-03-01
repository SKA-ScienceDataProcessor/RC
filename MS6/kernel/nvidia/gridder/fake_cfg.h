#ifndef __FAKE_CFG_H
#define __FAKE_CFG_H

#if defined __CUDACC__ || defined __CUDA__

#define __HOST_CONFIG_H__
#define _CRTIMP
#include <features.h>
#undef __CUDA_RUNTIME_H__
#include <cuda_runtime.h>

#endif

#endif
