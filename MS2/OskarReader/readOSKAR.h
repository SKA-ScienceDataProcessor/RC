/* readOSKAR.h

  Function to read data from OSKAR vis file.

  Reads and reshuffles u, v, w, amp data to the
  layout, suitable for Romein (R) and Halide (H) gridders consumption.

  Copyright (C) 2014 Braam Research, LLC.
 */

#include "o2a.h"

#ifdef __cplusplus
extern "C" {
#endif

enum rerrors {
    ro_ok                    =  0
  , ro_cant_open_vis_file    = -1
  , ro_invalid_data          = -2
  , ro_cant_allocate_amp_mem = -3
  , ro_cant_allocate_uvw_mem = -4
};

typedef struct OSKARdata_tag {
  // Let modify inplace if necessary.
  Double4c * amp;
  Double3 * uvw;
  int amp_num; // items count, not byte
  int uvw_num; // items count, not byte
} OSKARdata;

int readOSKARdata(const char * filepath, OSKARdata * od);
int finalizeOSKARdata(OSKARdata * od);

#ifdef __cplusplus
}
#endif
