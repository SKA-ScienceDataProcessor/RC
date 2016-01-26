#ifndef HALIDE_ATTRIBUTE_ALIGN
  #ifdef _MSC_VER
    #define HALIDE_ATTRIBUTE_ALIGN(x) __declspec(align(x))
  #else
    #define HALIDE_ATTRIBUTE_ALIGN(x) __attribute__((aligned(x)))
  #endif
#endif
#ifndef BUFFER_T_DEFINED
#define BUFFER_T_DEFINED
#include <stdbool.h>
#include <stdint.h>
typedef struct buffer_t {
    uint64_t dev;
    uint8_t* host;
    int32_t extent[4];
    int32_t stride[4];
    int32_t min[4];
    int32_t elem_size;
    HALIDE_ATTRIBUTE_ALIGN(1) bool host_dirty;
    HALIDE_ATTRIBUTE_ALIGN(1) bool dev_dirty;
    HALIDE_ATTRIBUTE_ALIGN(1) uint8_t _padding[10 - sizeof(void *)];
} buffer_t;
#endif
struct halide_filter_metadata_t;
#ifndef HALIDE_FUNCTION_ATTRS
#define HALIDE_FUNCTION_ATTRS
#endif
#ifdef __cplusplus
extern "C" {
#endif
int find_peak_cpu(buffer_t *_res_buffer, buffer_t *_findPeak_0_buffer, buffer_t *_findPeak_1_buffer, buffer_t *_findPeak_2_buffer) HALIDE_FUNCTION_ATTRS;
int find_peak_cpu_argv(void **args) HALIDE_FUNCTION_ATTRS;
extern const struct halide_filter_metadata_t find_peak_cpu_metadata;
int res_cpu(buffer_t *_res_buffer, buffer_t *_psf_buffer, const double _p0, const int32_t _p1, const int32_t _p2, buffer_t *_residual_buffer, buffer_t *_peakVal_buffer) HALIDE_FUNCTION_ATTRS;
int res_cpu_argv(void **args) HALIDE_FUNCTION_ATTRS;
extern const struct halide_filter_metadata_t res_cpu_metadata;
int model_cpu(buffer_t *_res_buffer, buffer_t *_psf_buffer, const double _p0, const int32_t _p1, const int32_t _p2, buffer_t *_model_buffer, buffer_t *_peakVal_buffer) HALIDE_FUNCTION_ATTRS;
int model_cpu_argv(void **args) HALIDE_FUNCTION_ATTRS;
extern const struct halide_filter_metadata_t model_cpu_metadata;
int resmodel_cpu(buffer_t *_res_buffer, buffer_t *_psf_buffer, const double _p0, const int32_t _p1, const int32_t _p2, buffer_t *_residual_buffer, buffer_t *_model_buffer, buffer_t *_peakVal_buffer) HALIDE_FUNCTION_ATTRS;
int resmodel_cpu_argv(void **args) HALIDE_FUNCTION_ATTRS;
extern const struct halide_filter_metadata_t resmodel_cpu_metadata;
#ifdef __cplusplus
}  // extern "C"
#endif

#include <cmath>
#include <cstring>

#define tohost(a) const_cast<uint8_t*>(reinterpret_cast<const uint8_t*>(a))

template<typename T> inline
buffer_t mkHalideBuf(int32_t size = 0){
  buffer_t buf = {0};
  buf.stride[0] = 1;
  buf.extent[0] = size;
  buf.elem_size = sizeof(T);
  return buf;
}

const unsigned int niters = 12;
const int bad_param_error = -24;

template <bool ismodel>
int deconvolve(
    const double gain
  , const double threshold
  , buffer_t * psf_buf_p
  , buffer_t * res_buf_p
  , buffer_t * mod_buf_p
  ) {
  int res = 0;
  #define __CK if (res != 0) return res;

  int psf_peakx, psf_peaky;
  buffer_t psf_peakx_buf, psf_peaky_buf;
  psf_peakx_buf = psf_peaky_buf = mkHalideBuf<int>(1);
  psf_peakx_buf.host = tohost(&psf_peakx);
  psf_peaky_buf.host = tohost(&psf_peaky);

  double peakval;
  buffer_t peakval_buf;
  peakval_buf = mkHalideBuf<double>(1);
  peakval_buf.host = tohost(&peakval);

  // In fact we throw away peakval here
  res = find_peak_cpu(psf_buf_p, &psf_peakx_buf, &psf_peaky_buf, &peakval_buf); __CK

  int (*kernel)(buffer_t *, buffer_t *, const double, const int32_t, const int32_t, buffer_t *, buffer_t *);
  if (ismodel) {
    memset(mod_buf_p->host, 0, mod_buf_p->stride[1] * mod_buf_p->extent[1] * mod_buf_p->elem_size);
    kernel = model_cpu;
  }
  else {
    if (res_buf_p != mod_buf_p) return bad_param_error;
    kernel = res_cpu;
  }

  for (unsigned int i = 0; i < niters; ++i) {
    res = kernel(res_buf_p, psf_buf_p, gain, psf_peakx, psf_peaky, mod_buf_p, &peakval_buf); __CK
    if (fabs(peakval) < threshold) break;
  }

  return res;
}

extern "C" {

int kern_hogbom_model   (const double peak, const double threshold, buffer_t * psf_buf_p, buffer_t * res_buf_p, buffer_t * mod_buf_p){
  return deconvolve<true>(peak, threshold, psf_buf_p, res_buf_p, mod_buf_p);
}

int kern_hogbom_residual(const double peak, const double threshold, buffer_t * psf_buf_p, buffer_t * res_buf_p, buffer_t * res_out_buf_p){
  return deconvolve<false>(peak, threshold, psf_buf_p, res_buf_p, res_out_buf_p);
}

}
