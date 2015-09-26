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

inline
tuple<buffer_t, buffer_t> mk1DHalideBuf(const complexd * v, int32_t size){
  buffer_t bufr = {0};
  bufr.host = tohh(& reinterpret_cast<const double (&)[2]>(v[0])[0]);
  bufr.stride[0] = 2; // generally nonportable
  bufr.extent[0] = size;
  bufr.elem_size = sizeof(double);

  buffer_t bufi = bufr;
  bufi.host = tohh(& reinterpret_cast<const double (&)[2]>(v[0])[1]);
  
  return make_tuple(bufr, bufi);
}

inline
tuple<buffer_t, buffer_t, buffer_t> mk1DHalideBuf(const Double3 * v, int32_t size){
  buffer_t bufx = {0};
  bufx.host = tohh(&v[0].u);
  bufx.stride[0] = 3; // generally nonportable
  bufx.extent[0] = size;
  bufx.elem_size = sizeof(double);

  buffer_t bufy = bufx;
  bufy.host = tohh(&v[0].v);

  buffer_t bufz = bufx;
  bufz.host = tohh(&v[0].w);

  return make_tuple(bufx, bufy, bufz);
}

template<typename T> inline
buffer_t mk1DHalideBuf(const T * v, int32_t size){
  buffer_t buf = {0};
  buf.host = tohh(&v[0]);
  buf.stride[0] = 1;
  buf.extent[0] = size;
  buf.elem_size = sizeof(T);
  return buf;
}

// 2D doublish
template<typename T> inline
buffer_t mk2DHalideBuf(const T * v, int32_t size){
  const int32_t n = sizeof(T)/sizeof(double);

  buffer_t buf = {0};
  buf.host = tohh(v);
  buf.stride[0] = 1;
  buf.extent[0] = n;
  buf.stride[1] = n;
  buf.extent[1] = size;
  buf.elem_size = sizeof(double);

  return buf;
}
