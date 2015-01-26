/* binner.cpp

  Reads and bins data from OSKAR vis file.

  Copyright (C) 2015 Braam Research, LLC.
 */

#include "binner.h"
#include "metrix.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cmath>

using namespace std;

typedef struct Point_tag
{
  double u;
  double v;
  double w;
  Double4c vis;
} Point;

inline double aff(double p, double coeff, double shift){
  return p * coeff + shift;
}

typedef unsigned int uint;

#ifdef SEPARATE_TIMESTEPS
// If we want to separate data for each timestep we stuck into the ulimit is 1024 for user.
// We could close some open files to free resources, but we can't start several thousands gridders
// during MS2 demo anyway.
// Hence see another option -- much simpler and with no optimizations (which are premature and are EVIL!)
struct filegrid {
  filegrid(uint chans, uint timesteps, uint divs) : last_ch(0), last_ts(0), last_off(0),
    d_gap(divs), ts_gap(d_gap*divs), ch_gap(ts_gap*timesteps), files(vector<FILE*>(ch_gap * chans, nullptr)) {;}

  ~filegrid(){for (FILE * f : files){if (f > nullptr) fclose(f);}}

  // void put_point(uint ch, uint ts, uint x, uint y, const Point & p){
  void put_point(uint ch, uint ts, uint x, uint y, const vis_data & p){
    uint off;
    if (ch == last_ch && ts == last_ts)
      off = last_off;
    else {
      last_ch = ch;
      last_ts = ts;
      off = ch * ch_gap + ts * ts_gap;
      last_off = off;
    }
    off += x*d_gap + y;
    if (files[off] == nullptr) {
      static char name[64];
      sprintf(name, "bins/%06u-%06u-%03u-%03u", ch, ts, x, y);
      files[off] = fopen(name, "wb");
      if (files[off] == nullptr) {
        printf("Gone bad at %s on %u with %d\n", name, off, errno);
        exit(1);
      }
    }
    fwrite(&p, 1, sizeof(p), files[off]);
  }

private:
  uint last_ch;
  uint last_ts;
  uint last_off;

  const uint d_gap;
  const uint ts_gap;
  const uint ch_gap;

  vector<FILE*> files;
};
#else
struct filegrid {
  filegrid(uint chans, uint divs) : divs(divs), files(vector<FILE*>(chans * divs * divs, nullptr)) {;}

  ~filegrid(){for (FILE * f : files){if (f > nullptr) fclose(f);}}

  // void put_point(uint ch, uint x, uint y, const Point & p){
  void put_point(uint ch, uint x, uint y, const vis_data & p){
    uint off = (ch * divs + x) * divs + y ;
    if (files[off] == nullptr) {
      static char name[64];
      sprintf(name, "bins/%06u-%03u-%03u", ch, x, y);
      files[off] = fopen(name, "wb");
      if (files[off] == nullptr) {
        printf("Gone bad at %s on %u with %d\n", name, off, errno);
        exit(1);
      }
    }
    fwrite(&p, 1, sizeof(p), files[off]);
  }

private:
  const uint divs;
  vector<FILE*> files;
};
#endif

const double GRID_STEP = double(GRID_D) / double(DIVIDERS);
const double HALF_SUPP = double(SUPP_D) / 2.0;
const double MARGIN = HALF_SUPP / GRID_STEP;

int main(int argc, char * argv[]) {

  if (argc < 2) {
    printf("usage: %s <visfile>\n", argv[0]);
  }

  VisHandle vh = vis_allocate_and_read(argv[1]);

  if (vh == NULL) {
    return ro_cant_open_vis_file;
  } else {
    DoubleData data = vis_get_doubledata(vh);
    if (__IS_VALID_DATA(data)) {
      Dimensions dims = vis_get_dimensions(vh);
      printf(
         "channels: %u\n"
         "timesteps: %u\n"
         "baselines: %u\n"
        , dims.channels
        , dims.timesteps
        , dims.baselines
        );

      double
          freq_start = vis_freq_start_hz(vh)
        , freq_inc = vis_freq_inc_hz(vh)
        ;

      int uvwnum = dims.timesteps * dims.baselines;

      const double
          maxu = * max_element(data.u, data.u + uvwnum)
        , minu = * min_element(data.u, data.u + uvwnum)
        , maxv = * max_element(data.v, data.v + uvwnum)
        , minv = * min_element(data.v, data.v + uvwnum)
        , maxw = * max_element(data.w, data.w + uvwnum)
        , minw = * min_element(data.w, data.w + uvwnum)
        ;

      double
          maxxu = max(maxu, -minu)
        , maxxv = max(maxv, -minv)
        , maxx = max(maxxu, maxxv)
        ;

      double
          max_freq = freq_start + freq_inc * double(dims.channels)
        , min_vawelength = SPEED_OF_LIGHT / max_freq
        , maxx_in_vawelengths = maxx / min_vawelength
        , uvw_scale = double(GRID_D) / (2.0 * maxx + 1.0) // No shift on support/2.
        , uv_shift = double(GRID_D) / 2.0
        , w_shift = -minw * uvw_scale
        // This additional 0.1 introduced to mitigate rounding errors and stay within WPLANES.
        , w_step = (maxw - minw) * uvw_scale / double(WPLANES) + 0.1
        ;

      printf(
         "max_freq: %f\n"
         "min_vawelength: %f\n"
         "maxx_in_vawelengths: %f\n"
         "uvw_scale: %f\n"
         "uv_shift: %f\n"
         "w_shift: %f\n"
         "w_step: %f\n"
        , max_freq
        , min_vawelength
        , maxx_in_vawelengths
        , uvw_scale
        , uv_shift
        , w_shift
        , w_step
        );

      
      // Amplitudes are in row-major
      //   [channels][timesteps][baselines][polarizations] array
      // U, V and W are in row-major
      //   [timesteps][baselines] array
      const double
          * cu = data.u
        , * cv = data.v
        , * cw = data.w
        ;
      const Double4c * camp = data.amp;

      const int chstep = dims.timesteps * dims.baselines;

      filegrid files(dims.channels, /* dims.timesteps, */ DIVIDERS);

      for (int ch = 0; ch < dims.channels; ch++) {
        int chbl0;
        chbl0 = chstep * ch;
        double cscale;
        cscale = 1.0 + freq_inc * double(ch) / freq_start;

        for (int ts = 0; ts < dims.timesteps; ts++) {
          int bl0;
          bl0 = ts * dims.baselines;
          for (int bl = 0; bl < dims.baselines; bl++) {
            int bloff;
            bloff = bl0 + bl;
            Double4c amp;
            amp = camp[chbl0 + bloff];
            double u, v, w, us, vs, ws;
            u = cu[bloff];
            v = cv[bloff];
            w = cw[bloff];
            us = cscale * aff(u, uvw_scale, uv_shift);
            vs = cscale * aff(v, uvw_scale, uv_shift);
            ws = cscale * aff(w, uvw_scale, w_shift);

            double
                usi, usf
              , vsi, vsf
              ;
            usf = modf(us/GRID_STEP, &usi);
            vsf = modf(vs/GRID_STEP, &vsi);

            int u_, v_, wplane_;
            short fracu_, fracv_;
            u_ = int(us);
            v_ = int(vs);
            wplane_ = int(ws / w_step);
            fracu_ = short(double(OVER) * (us - double(u_)));
            fracv_ = short(double(OVER) * (vs - double(v_)));

            // Point p;
            // p = {us, vs, ws, amp};
            vis_data p;
            p = {u_, v_, wplane_, fracu_, fracv_, amp};
            assert(int(usi) >= 0 && int(vsi) >= 0);
            assert(uint(usi) < DIVIDERS && uint(vsi) < DIVIDERS);
            files.put_point(ch, /* ts, */ uint(usi), uint(vsi), p);

#ifndef NO_MARGINS
            // Now we should also check if the point lies in the "margins" of adjacent bins

            bool leftm, rightm, topm, botm;
            leftm = false; rightm = false; topm = false; botm = false;

            if (usf < MARGIN && usi >= 1.0) {
              files.put_point(ch, /* ts, */ uint(usi-1.0), uint(vsi), p);
              leftm = true;
            }
            else if (usf > (1.0 - MARGIN) && usi < double (DIVIDERS-1)) {
              files.put_point(ch, /* ts, */ uint(usi+1.0), uint(vsi), p);
              rightm = true;
            }

            if (vsf < MARGIN && vsi >= 1.0) {
              files.put_point(ch, /* ts, */ uint(usi), uint(vsi-1.0), p);
              botm = true;
            }
            else if (vsf > (1.0 - MARGIN) && vsi < double (DIVIDERS-1)) {
              files.put_point(ch, /* ts, */ uint(usi), uint(vsi+1.0), p);
              topm = true;
            }

            if (leftm && botm)
              files.put_point(ch, /* ts, */ uint(usi-1.0), uint(vsi-1.0), p);
            else if (leftm && topm)
              files.put_point(ch, /* ts, */ uint(usi-1.0), uint(vsi+1.0), p);
            else if (rightm && topm)
              files.put_point(ch, /* ts, */ uint(usi+1.0), uint(vsi+1.0), p);
            else if (rightm && botm)
              files.put_point(ch, /* ts, */ uint(usi+1.0), uint(vsi-1.0), p);
#endif
          }
        }
      }
    }
  }
}
