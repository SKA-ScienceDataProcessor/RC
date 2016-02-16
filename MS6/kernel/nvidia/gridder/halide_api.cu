#include "grid_gpu.cuh"
#include "halide_buf.h"

extern "C"
int nvGridder(const double _scale, const int32_t _grid_size, buffer_t *_vis_buffer, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer){
  // FIXME: make gridGPUx respect buffers locations, i.e.
  // to check if they already are on GPU and skip allocation and marshaling then
  gridGPUc( _scale
          , reinterpret_cast<CmplxType*>(_uvg_buffer->host)
          , reinterpret_cast<combined*>(_vis_buffer->host)
          , _vis_buffer->extent[1]
          , _grid_size 
          , reinterpret_cast<CmplxType*>(_gcf_buffer->host)
          );
  _vis_buffer->dev = 0;
  _vis_buffer->host_dirty = false;
  _gcf_buffer->dev = 0;
  _gcf_buffer->host_dirty = false;

  _uvg_buffer->dev = 0;
  _uvg_buffer->host_dirty = false;
  _uvg_buffer->dev_dirty = false;

  return 0;
}
