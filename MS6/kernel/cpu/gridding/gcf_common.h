#ifndef __GCF_COMMON_H
#define __GCF_COMMON_H

#include <string>
#include "halide_buf.h"

inline
int32_t checkSize(const buffer_t & gcf) {
  int32_t size = gcf.extent[1];
  if (  gcf.extent[0] == 2
     && gcf.extent[2] == size
     ) return size;
  return -1;
}

#endif
