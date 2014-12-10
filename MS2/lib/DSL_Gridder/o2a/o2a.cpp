/* o2a.cpp

  Oskar visibilites file data reading library.
  This is a relatively thin layer on top of Oskar API.

  We choose to isolate user from *any* of Oskar compile-time dependencies.
  Only link-time dependency remains -- we shall link client programs against liboskar.a

  Copyright (C) 2014 Braam Research, LLC.
 */

#include "o2a.h"

#include <oskar_log.h>
#include <oskar_get_error_string.h>
#include <oskar_vis.h>
#include <oskar_vector_types.h>
#include <oskar_version_string.h>

inline VisHandle _(oskar_Vis * p){ return reinterpret_cast<VisHandle>(p); }
inline oskar_Vis * _(VisHandle h){ return reinterpret_cast<oskar_Vis *>(h); }

VisHandle vis_allocate_and_read(const char * filepath){
  int status = 0;
  auto vh = oskar_vis_read(filepath, &status);
  // We return no status to the outer world
  // because for any error we tried it returns -3.
  // FIXME! But we didn't try memory allocation error.
  if (status != 0) return nullptr;
  else return _(vh);
}

int vis_free(VisHandle vh){
  int status = 0;
  oskar_vis_free(_(vh), &status);
  return status;
}

Dimensions vis_get_dimensions(const VisHandle vh){
  return {
      oskar_vis_num_channels(_(vh))
    , oskar_vis_num_times(_(vh))
    , oskar_vis_num_baselines(_(vh))
    , oskar_vis_num_pols(_(vh))
    };
}

FloatData vis_get_floatdata(const VisHandle vh){
  const oskar_Mem* uu = oskar_vis_baseline_uu_metres_const(_(vh));

  int type = oskar_mem_type(uu);
  if (type != OSKAR_SINGLE)
    return {
        nullptr
      , nullptr
      , nullptr
      , nullptr
      };

  // We ignore statuses because already checked the type above.
  int status = 0;

  return {
      oskar_mem_float_const(uu, &status)
    , oskar_mem_float_const(
          oskar_vis_baseline_vv_metres_const(_(vh))
        , &status
        )
    , oskar_mem_float_const(
          oskar_vis_baseline_ww_metres_const(_(vh))
        , &status
        )
    , reinterpret_cast<const Float4c*>(
        oskar_mem_float4c_const(
            oskar_vis_amplitude_const(_(vh))
          , &status
          )
        )
  };
}

DoubleData vis_get_doubledata(const VisHandle vh){
  const oskar_Mem* uu = oskar_vis_baseline_uu_metres_const(_(vh));

  int type = oskar_mem_type(uu);
  if (type != OSKAR_DOUBLE)
    return {
        nullptr
      , nullptr
      , nullptr
      , nullptr
      };

  // We ignore statuses because already checked the type above.
  int status = 0;

  return {
      oskar_mem_double_const(uu, &status)
    , oskar_mem_double_const(
          oskar_vis_baseline_vv_metres_const(_(vh))
        , &status
        )
    , oskar_mem_double_const(
          oskar_vis_baseline_ww_metres_const(_(vh))
        , &status
        )
    , reinterpret_cast<const Double4c*>(
        oskar_mem_double4c_const(
            oskar_vis_amplitude_const(_(vh))
          , &status
          )
        )
  };
}

int vis_num_stations(const VisHandle vh) {
  return oskar_vis_num_stations(_(vh));
}

double vis_freq_start_hz(const VisHandle vh) {
  return oskar_vis_freq_start_hz(_(vh));
}

double vis_freq_inc_hz(const VisHandle vh) {
  return oskar_vis_freq_inc_hz(_(vh));
}
