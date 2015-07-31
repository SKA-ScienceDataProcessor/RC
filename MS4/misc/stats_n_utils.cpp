/* Copyright (C) 2015 Braam Research, LLC.
 */

#include "common.h"
#include "stats_n_utils.h"

void mkBlWpMap(const WMaxMin * bl_ws, int numBaselines, double wstep, BlWMap * bl_wis)
{
  double cmax, cmin;
  for (int i = 0; i < numBaselines; i++) {
    cmax = bl_ws[i].maxw/wstep;
    cmin = bl_ws[i].minw/wstep;
    bl_wis[i].bl = i;
    /*
    if (cmax > 0.0)
      bl_wis[i].wp = round(cmax);
    else if (cmin < 0.0)
      bl_wis[i].wp = round(cmin);
    else
      bl_wis[i].wp = 0;
     */
    // Use minimum instead of maximum
    // to make things easier at the moment,
    // because this determines max support handled by
    // threadblock in Romein-like algorithm and if it
    // is greater than that determined by current data point
    // we point to nonsensical GCF data.
    if (cmax < 0.0)
      bl_wis[i].wp = int(round(cmax));
    else if (cmin > 0.0)
      bl_wis[i].wp = int(round(cmin));
    else
      bl_wis[i].wp = 0;
  }
}

/*
long long count_points(BlWMap * m, int n){
  long long res = 0;
  for(int i = 0; i < n; i++) {
    int supp = get_supp(m[i].wp);
    res += supp * supp;
  }
  return res;
} */
