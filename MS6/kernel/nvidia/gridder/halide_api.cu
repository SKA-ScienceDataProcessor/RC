#include "grid_gpu.cuh"
#include "halide_buf.h"

extern "C"
int nvGridder(const double _scale, const int32_t _grid_size, buffer_t *_vis_buffer, buffer_t *_gcf_buffer, buffer_t *_uvg_buffer){
  gridGPUc( _scale
          , reinterpret_cast<CmplxType*>(_uvg_buffer->host)
          , reinterpret_cast<combined*>(_vis_buffer->host)
          , _vis_buffer->extent[1]
          , _grid_size 
          , reinterpret_cast<CmplxType*>(_gcf_buffer->host)
          );
  return 0;
}
