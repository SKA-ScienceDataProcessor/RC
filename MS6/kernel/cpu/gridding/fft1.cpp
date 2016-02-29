#include "halide_buf.h"

inline int32_t checkSize(const buffer_t & b_real, const buffer_t & b_cmplx) {
  int32_t size = b_real.extent[0];
  if (  b_real.extent[1] == size
     && b_cmplx.extent[0] == 2
     && b_cmplx.extent[1] == size
     && b_cmplx.extent[2] == size
     ) return size;
  return -1;
}

extern "C" {
int kern_ifft_1024x1024(buffer_t *_uvg_buffer, buffer_t *_img_shifted_buffer);
int kern_ifft_2048x2048(buffer_t *_uvg_buffer, buffer_t *_img_shifted_buffer);
int kern_ifft_3072x3072(buffer_t *_uvg_buffer, buffer_t *_img_shifted_buffer);
int kern_ifft_4096x4096(buffer_t *_uvg_buffer, buffer_t *_img_shifted_buffer);
int kern_ifft_6144x6144(buffer_t *_uvg_buffer, buffer_t *_img_shifted_buffer);

int kern_fft_1024x1024(buffer_t *_image_buffer, buffer_t *_uvg_herm_buffer);
int kern_fft_2048x2048(buffer_t *_image_buffer, buffer_t *_uvg_herm_buffer);
int kern_fft_3072x3072(buffer_t *_image_buffer, buffer_t *_uvg_herm_buffer);
int kern_fft_4096x4096(buffer_t *_image_buffer, buffer_t *_uvg_herm_buffer);
int kern_fft_6144x6144(buffer_t *_image_buffer, buffer_t *_uvg_herm_buffer);


int kern_ifft(buffer_t *_uvg_buffer, buffer_t *_img_shifted_buffer){
  int32_t size = checkSize(*_img_shifted_buffer, *_uvg_buffer);
  #define __I_CASE(siz) case siz: return kern_ifft_ ## siz ## x ## siz (_uvg_buffer, _img_shifted_buffer);
  switch( size ) {
    __I_CASE(2048)
    __I_CASE(3072)
    __I_CASE(6144)
    __I_CASE(1024)
    __I_CASE(4096)
  }
  return -666;
}

int kern_fft(buffer_t *_image_buffer, buffer_t *_uvg_herm_buffer){
  int32_t size = checkSize(*_image_buffer, *_uvg_herm_buffer);
  #define __R_CASE(siz) case siz: return kern_fft_ ## siz ## x ## siz (_image_buffer, _uvg_herm_buffer);
  switch( size ) {
    __R_CASE(2048)
    __R_CASE(3072)
    __R_CASE(6144)
    __R_CASE(1024)
    __R_CASE(4096)
  }
  return -666;
}

}
