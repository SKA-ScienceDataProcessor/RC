#include <array>
#include "Halide.h"

using namespace std;

#define tohh(a) const_cast<uint8_t*>(reinterpret_cast<const uint8_t*>(a))

inline void centerHalideBuf(buffer_t * bp, int nelems){
  bp->host += bp->elem_size * nelems;
  bp->min[0] -= nelems;
}

template<int n, typename T>
inline
array<buffer_t, n> mkInterleavedHalideBufs(int32_t size){
  buffer_t buf = {0};
  buf.stride[0] = n;
  buf.extent[0] = size;
  buf.elem_size = sizeof(T);

  array<buffer_t, n> res;
  for(int i=0; i<n; i++) res[i]=buf;
  return res;
}

template <int n, typename T>
inline
void setInterleavedHalideBufs(const T * v, array<buffer_t, n> & bufs){
  for(int i=0; i<n; i++) bufs[i].host= tohh(v+i);
}

template<int n, typename T>
inline
array<buffer_t, n> mkInterleavedHalideBufs(const T * v, int32_t size){
  auto bufs = mkInterleavedHalideBufs<n, T>(size);
  setInterleavedHalideBufs<n, T>(v, bufs);
  return bufs;

}

template<typename T> inline
void setHalideBuf(const T * v, buffer_t & buf){
  buf.host = tohh(v);
}

template<typename T> inline
buffer_t mkHalideBuf(int32_t size){
  buffer_t buf = {0};
  buf.stride[0] = 1;
  buf.extent[0] = size;
  buf.elem_size = sizeof(T);
  return buf;
}

template<typename T> inline
buffer_t mkHalideBuf(const T * v, int32_t size){
  buffer_t buf = mkHalideBuf<T>(size);
  setHalideBuf(v, buf);
  return buf;
}

template<int n, typename T> inline
buffer_t mkHalideBuf(int32_t size){
  buffer_t buf = {0};
  buf.stride[0] = 1;
  buf.extent[0] = n;
  buf.stride[1] = n;
  buf.extent[1] = size;
  buf.elem_size = sizeof(T);
  return buf;
}

template<int n, typename T> inline
buffer_t mkHalideBuf(const T * v, int32_t size){
  buffer_t buf = mkHalideBuf<n, T>(size);
  setHalideBuf(v, buf);
  return buf;
}

template<int n, typename T> inline
buffer_t mkHalideBuf(int32_t size2, int32_t size){
  buffer_t buf = {0};
  buf.stride[0] = 1;
  buf.extent[0] = n;
  buf.stride[1] = n;
  buf.extent[1] = size;
  buf.stride[2] = n*size;
  buf.extent[2] = size2;
  buf.elem_size = sizeof(T);
  return buf;
}

template<int n, typename T> inline
buffer_t mkHalideBufPadded(int32_t size, int32_t pad){
  buffer_t buf = {0};
  buf.stride[0] = 1;
  buf.extent[0] = n;
  buf.stride[1] = n;
  buf.extent[1] = size;
  buf.stride[2] = n*(size+pad);
  buf.extent[2] = size;
  buf.elem_size = sizeof(T);
  return buf;
}

template<int n, typename T> inline
buffer_t mkHalideBufPadded(const T * v, int32_t size, int32_t pad){
  buffer_t buf = mkHalideBufPadded<n, T>(size, pad);
  setHalideBuf(v, buf);
  return buf;
}

template<int n, typename T> inline
buffer_t mkHalideBuf(const T * v, int32_t size2, int32_t size){
  buffer_t buf = mkHalideBuf<n, T>(size2, size);
  setHalideBuf(v, buf);
  return buf;
}
