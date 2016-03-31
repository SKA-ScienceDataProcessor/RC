#include "gcf_common.h"

extern "C" {
int kern_degrid_8(const double _scale, const int32_t _grid_size, const int32_t _margin_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer);
int kern_degrid_16(const double _scale, const int32_t _grid_size, const int32_t _margin_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer);
int kern_degrid_32(const double _scale, const int32_t _grid_size, const int32_t _margin_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer);
int kern_degrid_64(const double _scale, const int32_t _grid_size, const int32_t _margin_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer);

int kern_degrid(const double _scale, const int32_t _grid_size, const int32_t _margin_size, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer, buffer_t *_vis_buffer, buffer_t *_vis_out_buffer) {
  int32_t size = checkSize(*_gcf_buffer);
  #define __I_CASE(siz) case siz: return kern_degrid_ ## siz (_scale, _grid_size, _margin_size, _gcf_buffer, _uvg_buffer, _vis_buffer, _vis_out_buffer);
  switch( size ) {
    __I_CASE( 8)
    __I_CASE(16)
    __I_CASE(32)
    __I_CASE(64)
  }
  return -555;
}

}
