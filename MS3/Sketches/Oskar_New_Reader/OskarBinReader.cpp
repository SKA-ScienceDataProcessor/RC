/* OskarBinReader.cpp

  Oskar 2.6.x binary file data reader.

  Reads and reshuffles amp and uvw data to the
  layout, suitable for Romein-like gridder consumption.

  Copyright (C) 2015 Braam Research, LLC.
 */

#include <algorithm>
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cmath>

#include <oskar_binary.h>

#include "OskarBinReader.h"

#if __GNUC__ == 4 && __GNUC_MINOR__ < 6
#define __OLD
#define nullptr NULL
#endif

#define SPEED_OF_LIGHT 299792458.0
// FIXME: Centralize definitions further (this is duplicated in CUDA code)
#define WPLANES 63

using namespace std;

// We temporarily use private tag enums, compatible
// with oskar-2.6 revision <= 2383 used for
// test dataset generation.
//
// When oskar-2.6 is released we should remove them
// and replace with
//    #include <oskar_vis_header.h>
//    #include <oskar_vis_block.h>
// and also add
//   OSKAR_VIS_HEADER_TAG_ and OSKAR_VIS_BLOCK_TAG_
// prefixes back (we removed them for clarity).

enum OSKAR_VIS_HEADER_TAGS
{
  TELESCOPE_PATH = 1,
  NUM_TAGS_PER_BLOCK = 2,
  WRITE_AUTOCORRELATIONS = 3,
  AMP_TYPE = 4,
  MAX_TIMES_PER_BLOCK = 5,
  NUM_TIMES_TOTAL = 6,
  NUM_CHANNELS = 7,
  NUM_STATIONS = 8,
  FREQ_START_HZ = 9,
  FREQ_INC_HZ = 10,
  CHANNEL_BANDWIDTH_HZ = 11,
  TIME_START_MJD_UTC = 12,
  TIME_INC_SEC = 13,
  TIME_AVERAGE_SEC = 14,
  PHASE_CENTRE = 15,
  TELESCOPE_CENTRE = 16,
  STATION_X_OFFSET_ECEF = 17,
  STATION_Y_OFFSET_ECEF = 18,
  STATION_Z_OFFSET_ECEF = 19
};

enum OSKAR_VIS_BLOCK_TAGS
{
  DIM_START_AND_SIZE = 1,
  FREQ_REF_INC_HZ = 2,
  TIME_REF_INC_MJD_UTC = 3,
  AUTO_CORRELATIONS = 4,
  CROSS_CORRELATIONS = 5,
  BASELINE_UU = 6,
  BASELINE_VV = 7,
  BASELINE_WW = 8
};

template <unsigned char> struct siz;
template<> struct siz<OSKAR_INT> { static const int val = sizeof(int); };
template<> struct siz<OSKAR_DOUBLE> { static const int val = sizeof(double); };
template<> struct siz<OSKAR_DOUBLE_COMPLEX_MATRIX> { static const int val = 2 * sizeof(double); };

// Variadic read
template<unsigned char data_type>
int bin_read(
  oskar_Binary*
  , unsigned char
  , int
  ) {
  return 0;
}

template<unsigned char data_type, typename... Args> int bin_read(
    oskar_Binary* h
  , unsigned char id_group
  , int user_index
  // these vary
  , unsigned char id_tag
  , int n
  , void * data
  , Args... args
  ) {
  int status = 0;
  oskar_binary_read(h
    , data_type
    , id_group
    , id_tag
    , user_index
    , n * siz<data_type>::val
    , data
    , &status);
  if (status != 0) return status;
  else return bin_read<data_type>(h, id_group, user_index, args...);
};
//

#define bin_read_d bin_read<OSKAR_DOUBLE>
#define bin_read_i bin_read<OSKAR_INT>

const unsigned char vis_header_group = OSKAR_TAG_GROUP_VIS_HEADER;
const unsigned char vis_block_group = OSKAR_TAG_GROUP_VIS_BLOCK;

#define __CHECK  if (status != 0) {if (h != nullptr) oskar_binary_free(h); return status;}

int mkFromFile(VisData * vdp, const char * filename) {
  oskar_Binary* h;
  int      
      num_baselines
    , num_channels
    , num_stations
    , num_times
    , num_times_baselines
    , num_points
    ;
  double
      phase_centre[2]
    , telescope_centre[3]
    , freq_start_inc[2]
    , time_start_inc[2]
    , channel_bandwidth_hz
    , time_average_sec
    ;

  int
      amp_type
    , status = 0
    ;
  h = oskar_binary_create(filename, 'r', &status);
  __CHECK

  status = bin_read_i(h, vis_header_group, 0
    , AMP_TYPE, 1, &amp_type
    , NUM_TIMES_TOTAL, 1, &num_times
    , NUM_CHANNELS, 1, &num_channels
    , NUM_STATIONS, 1, &num_stations
    );
  __CHECK

  if ((amp_type & 0x0F) != OSKAR_DOUBLE) {
    printf("Invalid data !\n");
    return int(0xdeadbeef);
  }

  status = bin_read_d(h, vis_header_group, 0
    , FREQ_START_HZ, 1, &freq_start_inc[0]
    , FREQ_INC_HZ, 1, &freq_start_inc[1]
    , CHANNEL_BANDWIDTH_HZ, 1, &channel_bandwidth_hz
    , TIME_START_MJD_UTC, 1, &time_start_inc[0]
    , TIME_INC_SEC, 1, &time_start_inc[1]
    , TIME_AVERAGE_SEC, 1, &time_average_sec
    , PHASE_CENTRE, 2, phase_centre
    , TELESCOPE_CENTRE, 3, telescope_centre
    );
  __CHECK

  num_baselines = num_stations * (num_stations - 1) / 2;
  num_times_baselines = num_times * num_baselines;
  num_points = num_times_baselines * num_channels;

#ifdef __OLD
  VisData r
#else
  *vdp
#endif
  = {
      h
    , num_baselines
    , num_channels
    , num_stations
    , num_times
    , num_times_baselines
    , num_points
    , { phase_centre[0]
      , phase_centre[1]
      }
    , { telescope_centre[0]
      , telescope_centre[1]
      , telescope_centre[2]
      }
    , { freq_start_inc[0]
      , freq_start_inc[1]
      }
    , { time_start_inc[0]
      , time_start_inc[1]
      }
    , channel_bandwidth_hz
    , time_average_sec
    };
#ifdef __OLD
  *vdp = r;
#endif

  return 0;
}

void freeBinHandler(VisData * vdp){
  oskar_binary_free(vdp->h);
}

#define __CHECK1(s) if (status != 0) {printf("ERROR! at %s: %d\n", #s, status); goto cleanup;}

int readAndReshuffle(const VisData * vdp, double * amps, double * uvws, Metrix * mp, WMaxMin * bl_ws, BlWMap * bl_wis)
{
  int status = 0;
  int
      max_times_per_block
    , num_tags_per_block
    , num_blocks
    , num_times_baselines_per_block
    ;
  double
      *u_temp = nullptr
    , *v_temp = nullptr
    , *w_temp = nullptr
    , *amp_temp = nullptr
    , *inv_lambdas = nullptr
    ;

    mp->maxu
  = mp->maxv
  = mp->maxw
  = -1e12;
    mp->minu
  = mp->minv
  = mp->minw
  =  1e12;
  
  for(int i = 0; i < vdp->num_baselines; i++) {
    bl_ws[i].maxw = -1e12;
    bl_ws[i].minw =  1e12;
  }

  status = bin_read_i(vdp->h, vis_header_group, 0
    , NUM_TAGS_PER_BLOCK, 1, &num_tags_per_block
    , MAX_TIMES_PER_BLOCK, 1, &max_times_per_block
    );
  __CHECK1(NUM_TAGS_AND_MAX_TIMES_PER_BLOCK)
  
  num_blocks = (vdp->num_times + max_times_per_block - 1) / max_times_per_block;
  num_times_baselines_per_block = max_times_per_block * vdp->num_baselines;

  double
      freq_start_inc[2]
    , time_start_inc[2]
    ; // local to block
  int dim_start_and_size[6];

  u_temp = (double *)malloc(num_times_baselines_per_block * DBL_SZ);
  v_temp = (double *)malloc(num_times_baselines_per_block * DBL_SZ);
  w_temp = (double *)malloc(num_times_baselines_per_block * DBL_SZ);
  amp_temp = (double *)malloc(num_times_baselines_per_block * vdp->num_channels * 8 * DBL_SZ);
  inv_lambdas = (double *)malloc(vdp->num_channels * DBL_SZ);

  /* Loop over blocks and read each one. */
  for (int block = 0; block < num_blocks; ++block)
  {
    int b, c, t, num_times_in_block, start_time_idx, start_channel_idx;

    /* Set search start index. */
    oskar_binary_set_query_search_start(vdp->h, block * num_tags_per_block, &status);

    /* Read block metadata. */
    status = bin_read_i(vdp->h, vis_block_group, block
      , DIM_START_AND_SIZE, 6, dim_start_and_size
      );
    __CHECK1(DIM_START_AND_SIZE)

    status = bin_read_d(vdp->h, vis_block_group, block
      , FREQ_REF_INC_HZ, 2, freq_start_inc
      , TIME_REF_INC_MJD_UTC, 2, time_start_inc
      );
    __CHECK1(FREQ_REF_INC_HZ + TIME_REF_INC_MJD_UTC)

    double cfreq;
    cfreq = freq_start_inc[0];
    for (c = 0; c < vdp->num_channels; c++, cfreq +=freq_start_inc[0]) {
      inv_lambdas[c] = cfreq / SPEED_OF_LIGHT;
    }

    /* Get the number of times actually in the block. */
    start_time_idx = dim_start_and_size[0];
    start_channel_idx = dim_start_and_size[1];
    num_times_in_block = dim_start_and_size[2];

    /* Read the visibility data. */
    status = bin_read<OSKAR_DOUBLE_COMPLEX_MATRIX>(vdp->h, vis_block_group, block
      , CROSS_CORRELATIONS, 4 * num_times_baselines_per_block * vdp->num_channels, amp_temp
      );
    __CHECK1(CROSS_CORRELATIONS)

    /* Read the baseline data. */
    status = bin_read_d(vdp->h, vis_block_group, block
      , BASELINE_UU, num_times_baselines_per_block, u_temp
      );
    __CHECK1(BASELINES)

    status = bin_read_d(vdp->h, vis_block_group, block
      , BASELINE_VV, num_times_baselines_per_block, v_temp
      );
    __CHECK1(BASELINES)

    status = bin_read_d(vdp->h, vis_block_group, block
      , BASELINE_WW, num_times_baselines_per_block, w_temp
      );
    __CHECK1(BASELINES)

    for (t = 0; t < num_times_in_block; ++t)
    {
      int tt;
      tt = start_time_idx + t;
      for (c = 0; c < vdp->num_channels; ++c)
      {
        int ct;
        ct = start_channel_idx + c;
        for (b = 0; b < vdp->num_baselines; ++b)
        {
          int i, j;
          int it, jt, tmp;
          // Amplitudes are in row-major
          //  [timesteps][channels][baselines][polarizations] array
          // U, V and W are in row-major
          //  [timesteps][baselines][uvw] array
          // And we want to convert them to
          //  [baselines][timesteps][channels][polarizations]
          // and
          //  [baselines][timesteps][uvw]
          // correspondingly
          i = 8 * (b + vdp->num_baselines * (c + vdp->num_channels * t));
          j = b + vdp->num_baselines * t;

          double u0, v0, w0;
            u0 = u_temp[j] * inv_lambdas[c];
            v0 = v_temp[j] * inv_lambdas[c];
            w0 = w_temp[j] * inv_lambdas[c];

          tmp = (vdp->num_times * b + tt) * vdp->num_channels + ct;
          jt = 3 * tmp;

          uvws[jt    ] = u0;
          uvws[jt + 1] = v0;
          uvws[jt + 2] = w0;

          mp->maxu = max(mp->maxu, u0);
          mp->maxv = max(mp->maxv, v0);
          mp->maxw = max(mp->maxw, w0);
          mp->minu = min(mp->minu, u0);
          mp->minv = min(mp->minv, v0);
          mp->minw = min(mp->minw, w0);
          bl_ws[b].maxw = max(bl_ws[b].maxw, w0);
          bl_ws[b].minw = min(bl_ws[b].minw, w0);

          it = 8 * tmp;
          for (int cc = 0; cc < 8; cc++) amps[it + cc] = amp_temp[i + cc];
        }
      }
    }

    double
        maxxw = max(mp->maxw, -mp->minw)
      , wstep = maxxw/(WPLANES/2)
      , cmax, cmin
      ;
    mp->wstep = wstep;
    mp->maxx = max(
          max(
            max(mp->maxu, -mp->minu)
          , max(mp->maxv, -mp->minv)
          )
        , maxxw
        );

    for (int i = 0; i < vdp->num_baselines; i++) {
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

  cleanup:
  /* Free local arrays. */
  if (amp_temp) free(amp_temp);
  if (u_temp) free(u_temp);
  if (v_temp) free(v_temp);
  if (w_temp) free(w_temp);
  if (inv_lambdas) free(inv_lambdas);

  return status;
}

struct __s1 {
  bool operator()(const BlWMap & l, const BlWMap & r) {
    return l.wp < r.wp;
  }
} cmp_plane;

struct __s2 {
  bool operator()(BlWMap l, BlWMap r) {
    if (l.wp < 0) l.wp = -l.wp;
    if (r.wp < 0) r.wp = -r.wp;
    return l.wp < r.wp;
  }
} cmp_abs;

void sort_on_w(BlWMap * bl_wis, int n) {
  sort(bl_wis, bl_wis + n, cmp_plane);
}

void sort_on_abs_w(BlWMap * bl_wis, int n) {
  sort(bl_wis, bl_wis + n, cmp_abs);
}
