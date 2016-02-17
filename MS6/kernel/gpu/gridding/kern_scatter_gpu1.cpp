#ifdef _MSC_VER
#pragma warning(push, 0)
#endif
#include "Halide.h"
#ifdef _MSC_VER
#pragma warning(pop)
#endif

using namespace Halide;

extern "C"
int kern_scatter_gpu(const double _scale, const int32_t _grid_size, buffer_t *_vis_buffer, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer);

#define __CK if (res != 0) {printf("Error: %d\n", res); return res; }

extern "C"
int kern_scatter_gpu1(const double _scale, const int32_t _grid_size, buffer_t *_vis_buffer, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer){
  int res;
  res = kern_scatter_gpu(_scale, _grid_size, _vis_buffer, _gcf_buffer, _uvg_buffer); __CK
  res = halide_device_sync(nullptr, _uvg_buffer); __CK
  res = halide_copy_to_host(nullptr, _uvg_buffer); __CK
  return 0;
}
