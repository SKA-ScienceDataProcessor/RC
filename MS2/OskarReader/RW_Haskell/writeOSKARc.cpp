/* writeOSKARc.cpp

  Function to read data from OSKAR vis file.

  Reshuffles u, v, w, amp data to the
  layout, suitable for Romein (R) and Halide (H) gridders consumption
  and writes them to client's buffers.

  Copyright (C) 2014 Braam Research, LLC.
 */

#include "writeOSKARc.h"

#include <stdlib.h>

int vis_write_and_free(VisHandle vh, Double4c * pamp, Double3 * puvw){
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
  const double *cu, *cv, *cw;

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

  vis_free(vh);
  return res;
}
