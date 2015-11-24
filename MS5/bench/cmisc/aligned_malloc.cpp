#ifndef _WIN32

#include <errno.h>
#include "aligned_malloc.h"

void * _aligned_malloc(
    size_t size
  , size_t alignment
  ) {
  void * p;
  errno = posix_memalign(&p, alignment, size);
  if (errno != 0) return NULL;
  return p;
}

#endif
