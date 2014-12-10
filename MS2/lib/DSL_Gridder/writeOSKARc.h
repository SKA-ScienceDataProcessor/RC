/* writeOSKARc.h

  Function to read data from OSKAR vis file.

  Reshuffles u, v, w, amp data to the
  layout, suitable for Romein (R) and Halide (H) gridders consumption
  and writes them to client's buffers.

  Copyright (C) 2014 Braam Research, LLC.
 */

#include "o2a.h"

#ifdef __cplusplus
extern "C" {
#endif

enum rerrors {
    ro_ok                    =  0
  , ro_invalid_data          = -1
  , ro_cant_allocate_amp_mem = -2
  , ro_cant_allocate_uvw_mem = -3
};

int vis_write_and_free(VisHandle vh, Double4c * pamp, Double3 * puvw);

#ifdef __cplusplus
}
#endif
