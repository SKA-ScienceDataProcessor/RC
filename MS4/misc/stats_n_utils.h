#ifndef __STATS_N_UTILS
#define __STATS_N_UTILS

#include "OskarBinReader.h"

typedef struct BlWMap_tag {
  int
      bl
    , wp
    ;
} BlWMap;

#ifdef __cplusplus
extern "C" {
#endif 

void mkBlWpMap(const WMaxMin * bl_ws, int numBaselines, double wstep, BlWMap * bl_wis);
// long long count_points(BlWMap * m, int n);

#ifdef __cplusplus
}
#endif 

#endif
