/* binner.h

  Reads and bins data from OSKAR vis file.

  Copyright (C) 2015 Braam Research, LLC.
 */

#include "o2a.h"

enum rerrors {
    ro_ok                    =  0
  , ro_cant_open_vis_file    = -1
  , ro_invalid_data          = -2
  , ro_cant_allocate_amp_mem = -3
  , ro_cant_allocate_uvw_mem = -4
};
