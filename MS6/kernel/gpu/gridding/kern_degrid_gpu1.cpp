#include "../../cpu/gridding/gcf_common.h"

#include <stdio.h>

extern "C" {
// Halide runtime
int halide_device_sync(void *user_context, struct buffer_t *buf);
int halide_copy_to_host(void *user_context, struct buffer_t *buf);
int halide_device_free(void *user_context, struct buffer_t *buf);

// Kernel generated
int kern_degrid_gpu_16(const double _scale, const int32_t _grid_size, const int32_t _margin_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer);
int kern_degrid_gpu_32(const double _scale, const int32_t _grid_size, const int32_t _margin_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer);
int kern_degrid_gpu_64(const double _scale, const int32_t _grid_size, const int32_t _margin_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer);

}

#define __CK if (res != 0) {printf("Error: %d\n", res); return res; }

extern "C"
int kern_degrid_gpu1(const double _scale, const int32_t _grid_size, const int32_t _margin_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer){
  int res;
  int32_t size = checkSize(*_gcf_buffer);
  #define __I_CASE(siz) case siz: res = kern_degrid_gpu_ ## siz (_scale, _grid_size, _margin_size, _gcf_buffer, _uvg_buffer, _vis_buffer, _vis_out_buffer); __CK break;
  switch( size ) {
    __I_CASE(16)
    __I_CASE(32)
    __I_CASE(64)
    default: return -333;
  }
  res = halide_device_sync(nullptr, _vis_out_buffer); __CK
  res = halide_copy_to_host(nullptr, _vis_out_buffer); __CK

  res = halide_device_free(nullptr, _gcf_buffer); __CK
  res = halide_device_free(nullptr, _uvg_buffer); __CK
  res = halide_device_free(nullptr, _vis_buffer); __CK
  res = halide_device_free(nullptr, _vis_out_buffer); __CK
  return 0;
}
