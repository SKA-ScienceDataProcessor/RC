#ifndef __ALIGNED_MALLOC_H
#define __ALIGNED_MALLOC_H

#ifndef _WIN32

#include <stdlib.h>

#ifdef __cplusplus
extern "C"
#endif
void * _aligned_malloc(size_t size, size_t alignment);
#define _aligned_free free

#else

#include <malloc.h>

#endif

#ifdef __cplusplus
template <typename t>
t * alignedMallocArray(size_t size, size_t alignment) {
  return reinterpret_cast<t*>(_aligned_malloc(size * sizeof(t), alignment));
}
#endif

#endif
