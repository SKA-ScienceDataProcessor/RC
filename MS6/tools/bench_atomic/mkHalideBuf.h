#define tohost(a) const_cast<uint8_t*>(reinterpret_cast<const uint8_t*>(a))

inline void centerHalideBuf(buffer_t * bp, int nelems){
  bp->host += bp->elem_size * nelems;
  bp->min[0] -= nelems;
}

template<typename T> inline
buffer_t mkHalideBuf(int32_t size = 0){
  buffer_t buf = {0};
  buf.stride[0] = 1;
  buf.extent[0] = size;
  buf.elem_size = sizeof(T);
  return buf;
}

#define __MKBUF(n,siz) buf.stride[n] = buf.stride[n-1] * buf.extent[n-1]; buf.extent[n] = siz;

template<typename T> inline
buffer_t mkHalideBuf(int32_t size2, int32_t size){
  buffer_t buf = mkHalideBuf<T>(size);
  __MKBUF(1, size2)
  return buf;
}

template<typename T> inline
buffer_t mkHalideBuf(int32_t size3, int32_t size2, int32_t size){
  buffer_t buf = mkHalideBuf<T>(size2, size);
  __MKBUF(2, size3)
  return buf;
}

template<typename T> inline
buffer_t mkHalideBuf(int32_t size4, int32_t size3, int32_t size2, int32_t size){
  buffer_t buf = mkHalideBuf<T>(size3, size2, size);
  __MKBUF(3, size4)
  return buf;
}
