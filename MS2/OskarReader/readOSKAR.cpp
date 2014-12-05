/* readOSKAR.h

  Function to read data from OSKAR vis file.

  Reads and reshuffles u, v, w, amp data to the
  layout, suitable for Romein (R) and Halide (H) gridders consumption.

  Copyright (C) 2014 Braam Research, LLC.
 */

#include "readOSKAR.h"

#include <malloc.h>

int finalizeOSKARdata(OSKARdata * od){
  free(od->amp);
  free(od->uvw);
  return 0;
};

int readOSKARdata(const char * filepath, OSKARdata * od) {
  VisHandle vh;
  Dimensions dims;
  DoubleData data;
  int
      ch, ts, bl
    , CH, TS, BL
    , CSTEP
    , bloff
    , res = ro_ok
    ;

  const Double4c *camp;
  Double4c * pamp;
  const double *cu, *cv, *cw;
  Double3 *puvw;

  vh = vis_allocate_and_read(filepath);

  if (vh == NULL) {
    return ro_cant_open_vis_file;
  } else {
    data = vis_get_doubledata(vh);
    if (__IS_VALID_DATA(data)) {
      dims = vis_get_dimensions(vh);

      // To be compatible with H/R shall swap
      // baselines, timesteps and channels dimensions.
      // No fancy optimizations. Plain stupid loops.
      BL = dims.baselines;
      TS = dims.timesteps;
      CH = dims.channels;
      CSTEP = TS*BL;
      camp = data.amp;
      cu = data.u;
      cv = data.v;
      cw = data.w;
      od->amp = reinterpret_cast<Double4c *>(malloc(CH * CSTEP * sizeof (*camp))); // alignment?
      if (od->amp == nullptr) {
        res = ro_cant_allocate_amp_mem;
        goto leave;
      }
      od->uvw = reinterpret_cast<Double3 *>(malloc(CSTEP * sizeof (*puvw))); // alignment?
      if (od->uvw == nullptr) {
        res = ro_cant_allocate_uvw_mem;
        free(od->amp);
        goto leave;
      }
      pamp = od->amp;
      puvw = od->uvw;
      for ( bl = 0; bl < BL; bl++ ) {
        for ( ts = 0; ts < TS; ts++ ) {
          bloff = BL * ts + bl;
          puvw->x = cu[bloff]; puvw->y = cv[bloff]; puvw->z = cw[bloff];
          puvw++;
          for ( ch = 0; ch < CH; ch++ ) {
            *pamp++ = camp[CSTEP * ch + bloff];
          }
        }
      }
    } else {
      res = ro_invalid_data;
    }

    leave:
    vis_free(vh);
    return res;
  }
}
