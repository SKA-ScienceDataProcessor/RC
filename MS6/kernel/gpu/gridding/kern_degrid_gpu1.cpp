#include <stdio.h>
#include "halide_buf.h"

extern "C" {
// Halide runtime
int halide_device_sync(void *user_context, struct buffer_t *buf);
int halide_copy_to_host(void *user_context, struct buffer_t *buf);

// Kernel generated
int kern_degrid_gpu(const double _scale, const int32_t _grid_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer);
}

#define __CK if (res != 0) {printf("Error: %d\n", res); return res; }

extern "C"
int kern_degrid_gpu1(const double _scale, const int32_t _grid_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer){
  int res;
  res = kern_degrid_gpu(_scale, _grid_size, _gcf_buffer, _uvg_buffer, _vis_buffer, _vis_out_buffer); __CK
  res = halide_device_sync(nullptr, _vis_out_buffer); __CK
  res = halide_copy_to_host(nullptr, _vis_out_buffer); __CK
  return 0;
}
