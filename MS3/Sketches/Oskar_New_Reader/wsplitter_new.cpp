/* wsplitter_new.cpp

  Reads data from OSKAR vis file and splits them on w-plane.
  Important: we keep an original order of data
  and store them in per-baseline way to keep them piecewise-continuous
  to make Romein work-distribution for GPUs happy.

  Copyright (C) 2015 Braam Research, LLC.
 */

#ifndef SPEED_OF_LIGHT
#define SPEED_OF_LIGHT 299792458.0
#endif

typedef struct Double2_tag
{
  double x;
  double y;
} Double2;

typedef struct Double3_tag
{
  double x;
  double y;
  double z;
} Double3;

typedef struct Double4c_tag {
  Double2 a;
  Double2 b;
  Double2 c;
  Double2 d;
} Double4c;

// #include "metrix.h"
#include "OskarBinReader.h"

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
int doit(const char * f) {
  VisData * vdp = mkFromFile(f);

  if (vdp == nullptr) {
    return -1;
  } else {
      printf(
         "channels: %u\n"
         "timesteps: %u\n"
         "baselines: %u\n"
        , vdp->num_channels
        , vdp->num_times
        , vdp->num_baselines
        );

      double
          freq_start = vdp->freq_start_inc[0]
        , freq_inc = vdp->freq_start_inc[1]
        ;

      Double3 * uvws = (Double3*)calloc(vdp->num_times_baselines, sizeof(Double3));
      Double4c * amps = (Double4c*)calloc(vdp->num_points, sizeof(Double4c));

      if (readAndReshuffle(vdp, (double*)amps, (double*)uvws) == 0) {

        double
            maxu = -1e8
          , maxv = -1e8
          , maxw = -1e8
          , minu = 1e8
          , minv = 1e8
          , minw = 1e8
          ;

        const Double3 * last_uvw = uvws + vdp->num_times_baselines;
        for(Double3 * uvwp = uvws; uvwp < last_uvw; uvwp++) {
           maxu = max(maxu, uvwp->x);
           maxv = max(maxu, uvwp->y);
           maxw = max(maxu, uvwp->z);
           minu = min(minu, uvwp->x);
           minv = min(minu, uvwp->y);
           minw = min(minu, uvwp->z);
        }

        double
            maxxu = max(maxu, -minu)
          , maxxv = max(maxv, -minv)
          , maxx = max(maxxu, maxxv)
          ;

        double
            max_freq = freq_start + freq_inc * double(vdp->num_channels)
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

        filegrid files(WPLANES);

        // We already have data reshuffled:
        // Amplitudes are in row-major
        //    [baselines][timesteps][channels][polarizations].
        // U, V and W are in row-major
        //    [baselines][timesteps][uvw].
        int bloff, tsoff, choff;
        for (int bl = 0; bl < vdp->num_baselines; bl++) {
          bloff = bl * vdp->num_times;
          for (int ts = 0; ts < vdp->num_times; ts++) {
            tsoff = (bloff + ts) * vdp->num_channels;
            double u, v, w;
            u = uvws[tsoff].x;
            v = uvws[tsoff].y;
            w = uvws[tsoff].z;
            for (int ch = 0; ch < vdp->num_channels; ch++) {
              choff = tsoff + ch;

              double cscale;
              cscale = 1.0 + freq_inc * double(ch) / freq_start;

              double us, vs, ws;
              us = cscale * aff(u, uvw_scale, uv_shift);
              vs = cscale * aff(v, uvw_scale, uv_shift);
              ws = cscale * aff(w, uvw_scale, w_shift);

              int wplane_;
              wplane_ = int(ws / w_step);

              t p;
#ifdef __NO_PREGRID
              p = {us, vs, ws, amps[choff]};
#else
              int u_, v_;
              short fracu_, fracv_;
              u_ = int(us);
              v_ = int(vs);
              fracu_ = short(double(OVER) * (us - double(u_)));
              fracv_ = short(double(OVER) * (vs - double(v_)));
              p = {u_, v_, wplane_, fracu_, fracv_, amps[choff]};
#endif
              files.put_point(wplane_, bl, p);
            }
          }
        }
      }
  }
  return 0;
}

int main(int argc, char * argv[]) {

  if (argc < 2) {
    printf("usage: %s <visfile>\n", argv[0]);
  }

  return doit<vis>(argv[1]);
}
