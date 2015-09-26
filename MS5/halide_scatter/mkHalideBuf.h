#include <tuple>
#include <complex>

#include "Halide.h"

using namespace std;

typedef complex<double> complexd;

struct Double3
{
  double u;
  double v;
  double w;
};

#define tohh(a) const_cast<uint8_t*>(reinterpret_cast<const uint8_t*>(a))

// FIXME: remove code repetition using variadic templates

inline
tuple<buffer_t, buffer_t> mk1DHalideBufComplexD(int32_t size){
  buffer_t buf = {0};
  buf.stride[0] = 2; // generally nonportable
  buf.extent[0] = size;
  buf.elem_size = sizeof(double);
  return make_tuple(buf, buf);
}

inline void set1D1DHalideBufComplexD(const complexd * v, tuple<buffer_t, buffer_t> & tup){
  get<0>(tup).host = tohh(& reinterpret_cast<const double (&)[2]>(v[0])[0]);
  get<1>(tup).host = tohh(& reinterpret_cast<const double (&)[2]>(v[0])[1]);
}

inline
tuple<buffer_t, buffer_t> mk1DHalideBuf(const complexd * v, int32_t size){
  auto tup = mk1DHalideBufComplexD(size);
  set1D1DHalideBufComplexD(v, tup);
  return tup;
}

inline
tuple<buffer_t, buffer_t, buffer_t> mk1DHalideBufDouble3(int32_t size){
  buffer_t buf = {0};
  buf.stride[0] = 3; // generally nonportable
  buf.extent[0] = size;
  buf.elem_size = sizeof(double);
  return make_tuple(buf, buf, buf);
}

inline void set1D1DHalideBufDouble3(const Double3 * v, tuple<buffer_t, buffer_t, buffer_t> & tup){
  get<0>(tup).host = tohh(&v[0].u);
  get<1>(tup).host = tohh(&v[0].v);
  get<2>(tup).host = tohh(&v[0].w);
}

inline
tuple<buffer_t, buffer_t, buffer_t> mk1DHalideBuf(const Double3 * v, int32_t size){
  auto tup = mk1DHalideBufDouble3(size);
  set1D1DHalideBufDouble3(v, tup);
  return tup;
}

template<typename T> inline
buffer_t mk1DHalideBuf(int32_t size){
  buffer_t buf = {0};
  buf.stride[0] = 1;
  buf.extent[0] = size;
  buf.elem_size = sizeof(T);
  return buf;
}

template<typename T> inline
void set1DHalideBuf(const T * v, buffer_t & buf){
  buf.host = tohh(&v[0]);
}

template<typename T> inline
buffer_t mk1DHalideBuf(const T * v, int32_t size){
  buffer_t buf = mk1DHalideBuf<T>(size);
  set1DHalideBuf(v, buf);
  return buf;
}

// 2D doublish
template<typename T> inline
buffer_t mk2DHalideBuf(int32_t size){
  const int32_t n = sizeof(T)/sizeof(double);

  buffer_t buf = {0};
  buf.stride[0] = 1;
  buf.extent[0] = n;
  buf.stride[1] = n;
  buf.extent[1] = size;
  buf.elem_size = sizeof(double);

  return buf;
}

template<typename T> inline
void set2DHalideBuf(const T * v, buffer_t & buf){
  buf.host = tohh(v);
}

template<typename T> inline
buffer_t mk2DHalideBuf(const T * v, int32_t size){
  buffer_t buf = mk2DHalideBuf<T>(size);
  set2DHalideBuf(v, buf);
  return buf;
}
