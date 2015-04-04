/* binner.cpp
  Copyright (C) 2015 Braam Research, LLC.
 */

#include "metrix.h"
#include "common.h"
#include "OskarBinReader.h"

#include <cassert>
#include <cstdio>
#include <cstring>
#include <sstream>

using namespace std;

template <
    int grid_size
  , int over
  , int w_planes
  , bool do_mirror
  >
inline void pregridPoint(double scale, Double3 uvw, Pregridded & res){
    uvw.u *= scale;
    uvw.v *= scale;
    uvw.w *= scale;
    short
        w_plane = short(round (uvw.w / (w_planes/2)))
      , max_supp = short(get_supp(w_plane))
      // We additionally translate these u v by -max_supp/2
      // because gridding procedure translates them back
      , u = short(round(uvw.u) + grid_size/2 - max_supp/2)
      , v = short(round(uvw.v) + grid_size/2 - max_supp/2)
      , over_u = short(round(over * (uvw.u - u)))
      , over_v = short(round(over * (uvw.v - v)))
      ;
    res.u = u;
    res.v = v;
    // Well, this is a kind of trick:
    // For full GCF we can have w_plane and hence gcf_layer_index negative.
    // But for half GCF we have both positive. Thus to convey an information
    // about original w being negative we negate the whole index.
    // When inspecting it client of half GCF if looking negative index should
    // both negate it *and* conjugate support pixel.
    if (do_mirror) {
      if (w_plane < 0) {
          res.gcf_layer_index = -((-w_plane * over + over_u) * over + over_v);
      } else {
          res.gcf_layer_index = (w_plane * over + over_u) * over + over_v;
      }
    } else {
      res.gcf_layer_index = (w_plane * over + over_u) * over + over_v;
    }
    res.gcf_layer_supp = max_supp;
}

typedef unsigned int uint;

template <int divs>
struct filegrid {

  int flush(const char * prefix) {
    for(int i = 0; i < divs; i++) {
      for(int j = 0; j < divs; j++) {
        if (pre_files[i][j].tellp() > 0) {
          static char name[512];
          char * last = name + sprintf(name, "%s%03u-%03u", prefix, i, j);

          FILE *f;
          char * buf;
          streamoff cnt;

          sprintf(last, "-pre.dat");
          f = fopen(name, "wb");
          if (f == nullptr) {
            printf("Gone bad at %s on (%u,%u) with %d\n", name, i, j, errno);
            return errno;
          }
          cnt = pre_files[i][j].tellp();
          buf = new char[cnt];
          pre_files[i][j].read(buf, cnt);
          fwrite(buf, 1, cnt, f);
          fclose(f);
          delete [] buf;

          sprintf(last, "-vis.dat");
          f = fopen(name, "wb");
          if (f == nullptr) {
            printf("Gone bad at %s on (%u,%u) with %d\n", name, i, j, errno);
            return errno;
          }
          cnt = vis_files[i][j].tellp();
          buf = new char[cnt];
          vis_files[i][j].read(buf, cnt);
          fwrite(buf, 1, cnt, f);
          fclose(f);
          delete [] buf;
        }
      }
    }
    return 0;
  }

  void put_point(uint x, uint y, const Pregridded & p, const Double4c & vis){
    pre_files[x][y].write((char*)&p, sizeof(p));
    vis_files[x][y].write((char*)&vis, sizeof(vis));
  }

private:
  stringstream pre_files[divs][divs];
  stringstream vis_files[divs][divs];
};

template <
    int grid_size
  , int over
  , int w_planes
  , int divs
  , bool do_mirror
  >
// We don't perform baselines sorting yet.
// But it is not so hard to implement it.
inline int doit(const char * prefix, int num_channels, int num_points, double scale, Double4c* amps, Double3 * uvws) {
  const int div_size = grid_size / divs;
  int res = 0;

  filegrid<divs> * filesp = new filegrid<divs>[num_channels];

  Double4c
      * amps_end = amps + num_points
    , * amp_curr = amps
    ;
  Double3 * uvw_curr = uvws;

  while(amp_curr < amps_end){
    for(int i = 0; i < num_channels; i++){
      Pregridded p;
      pregridPoint<grid_size, over, w_planes, do_mirror>(scale, *uvw_curr, p); // p is passed as reference and updated!
      div_t us, vs;
      // NOTE: we have u and v translated by -p.gcf_layer_supp/2
      // in pregridPoint, thus we temporarily put them back.
      us = div(p.u + p.gcf_layer_supp/2, div_size);
      vs = div(p.v + p.gcf_layer_supp/2, div_size);

      assert(us.quot >= 0 && vs.quot >= 0
          && us.quot < divs && vs.quot < divs
          );

      filesp[i].put_point(us.quot, vs.quot, p, *amp_curr);

      int margin;
      margin = p.gcf_layer_supp / 2;
      // Optimize slightly for most inhabited w-plane
      if (margin > 0) {
        bool leftm, rightm, topm, botm;
        leftm = false; rightm = false; topm = false; botm = false;

        if (us.rem < margin && us.quot >= 1) {
          filesp[i].put_point(us.quot-1, vs.quot, p, *amp_curr);
          leftm = true;
        }
        else if (us.rem > 1 - margin && us.quot < divs-1) {
          filesp[i].put_point(us.quot+1, vs.quot, p, *amp_curr);
          rightm = true;
        }

        if (vs.rem < margin && vs.quot >= 1) {
          filesp[i].put_point(us.quot, vs.quot-1, p, *amp_curr);
          botm = true;
        }
        else if (vs.rem > 1 - margin && vs.quot < divs-1) {
          filesp[i].put_point(us.quot, vs.quot+1, p, *amp_curr);
          topm = true;
        }

        if (leftm && botm) {
          filesp[i].put_point(us.quot-1, vs.quot-1, p, *amp_curr);
        }
        else if (leftm && topm) {
          filesp[i].put_point(us.quot-1, vs.quot+1, p, *amp_curr);
        }
        else if (rightm && topm) {
          filesp[i].put_point(us.quot+1, vs.quot+1, p, *amp_curr);
        }
        else if (rightm && botm) {
          filesp[i].put_point(us.quot+1, vs.quot-1, p, *amp_curr);
        }
      }

      amp_curr++;
      uvw_curr++;
    }
  }
  printf("Binning is done. Writing...\n");
  for(int i = 0; i < num_channels; i++){
    char new_prefix[256];
    snprintf(new_prefix, 256, "%s%06d-", prefix, i);
    res = filesp[i].flush(new_prefix);
    if (res != 0) break;
  }

  delete [] filesp;
  return res;
}

extern "C" {
int bin(const char * prefix, int num_channels, int num_points, double scale, Double4c* amps, Double3 * uvws) {
  return doit<GRID_SIZE, OVER, WPLANES, DIVIDERS, false>(prefix, num_channels, num_points, scale, amps, uvws);
}
int binm(const char * prefix, int num_channels, int num_points, double scale, Double4c* amps, Double3 * uvws) {
  return doit<GRID_SIZE, OVER, WPLANES, DIVIDERS, true>(prefix, num_channels, num_points, scale, amps, uvws);
}
}
