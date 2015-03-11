/* wsplitter.cpp

  Reads data from OSKAR vis file and splits them on w-plane.
  Important: we keep an original order of data
  and store them in per-baseline way to keep them piecewise-continuous
  to make Romein work-distribution for GPUs happy.

  Copyright (C) 2015 Braam Research, LLC.
 */

#include "binner.h"
#include "metrix.h"

#include <list>
#include <algorithm>
#include <cstdio>

using namespace std;

inline double aff(double p, double coeff, double shift){
  return p * coeff + shift;
}

struct baseline_descr {
  int bl;
  int items;
};

const int FRESH = -1;

struct wfile {
  wfile()
    : file(nullptr)
    , curr({FRESH, 0})
    , baseline_descriptions(list<baseline_descr>())
    {;}

  void start_baseline(int bl) {
    curr.bl = bl;
    curr.items = 0;
  }

  template <typename t>
  void put_point(const t & p) {
    fwrite(&p, 1, sizeof(p), file);
    curr.items++;
  }

  void finish_baseline() {
    baseline_descriptions.push_back(curr);
  }

  // data
  FILE * file;
  baseline_descr curr;
  list<baseline_descr> baseline_descriptions;
};

struct filegrid {
  filegrid(int wplanes)
    : files(vector<wfile>(wplanes)) {;}

  ~filegrid(){
    for (wfile f : files)
      if (f.file != nullptr) {
        f.finish_baseline();
        int size = 0; // evil! premature optimization!
        for(auto bld : f.baseline_descriptions) {
          fwrite(&bld, 1, sizeof(bld), f.file);
          size++;
        }
        fwrite(&size, 1, sizeof(size), f.file);
        fclose(f.file);
      }
  }

  template <typename t>
  void put_point(int wplane, int baseline, const t & p){
    if (files[wplane].file == nullptr) {
      static char name[64];
      sprintf(name, "wplanes/%02u_wplane.dat", wplane);
      files[wplane].file = fopen(name, "wb");
      if (files[wplane].file == nullptr) {
        printf("Gone bad at %s with %d\n", name, errno);
        exit(1);
      }
    }
    if (files[wplane].curr.bl != baseline) {
      if (files[wplane].curr.bl != FRESH) files[wplane].finish_baseline();
      files[wplane].start_baseline(baseline);
    }
    files[wplane].put_point(p);
  }

private:
  vector<wfile> files;
};

template <typename t>
rerrors doit(const char * f) {
  VisHandle vh = vis_allocate_and_read(f);

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
        , uvw_scale = double(GRID_D) / (2.0 * maxx * max_freq / freq_start + 1.0) // No shift on support/2.
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

      filegrid files(WPLANES);

      for (int bl = 0; bl < dims.baselines; bl++) {

        for (int ch = 0; ch < dims.channels; ch++) {
          int chbl0;
          chbl0 = chstep * ch;
          double cscale;
          cscale = 1.0 + freq_inc * double(ch) / freq_start;

          for (int ts = 0; ts < dims.timesteps; ts++) {
            int bl0;
            bl0 = ts * dims.baselines;

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

            int wplane_;
            wplane_ = int(ws / w_step);

            t p;
#ifdef __NO_PREGRID
            p = {us, vs, ws, amp};
#else
            int u_, v_;
            short fracu_, fracv_;
            u_ = int(us);
            v_ = int(vs);
            fracu_ = short(double(OVER) * (us - double(u_)));
            fracv_ = short(double(OVER) * (vs - double(v_)));
            p = {u_, v_, wplane_, fracu_, fracv_, amp};
#endif
            files.put_point(wplane_, bl, p);

          }
        }
      }
    }
  }
  return ro_ok;
}

int main(int argc, char * argv[]) {

  if (argc < 2) {
    printf("usage: %s <visfile>\n", argv[0]);
  }

  return doit<vis>(argv[1]);
}
