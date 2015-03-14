/* OskarBinReader.cpp

  Oskar 2.6.x binary file data reader.

  Reads and reshuffles amp and uvw data to the
  layout, suitable for Romein-like gridder consumption.

  Copyright (C) 2015 Braam Research, LLC.
 */

#include <cstdio>
#include <cstring>
#include <cstdlib>

#include <oskar_binary.h>

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

#define DBL_SZ sizeof(double)

template <unsigned char> struct siz;
template<> struct siz<OSKAR_INT> { static const int val = sizeof(int); };
template<> struct siz<OSKAR_DOUBLE> { static const int val = sizeof(double); };
template<> struct siz<OSKAR_DOUBLE_COMPLEX_MATRIX> { static const int val = 2 * sizeof(double); };

// Variadic read
template<unsigned char data_type>
int bin_read(
  oskar_Binary* h
  , unsigned char id_group
  , int user_index
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
#define __CHECK  if (status != 0) {h = nullptr; return status;}

struct VisData {
  int mk_from_file(const char* filename) {
    int status = 0;
    h = oskar_binary_create(filename, 'r', &status);
    __CHECK

    status = bin_read_i(h, vis_header_group, 0
      , NUM_TAGS_PER_BLOCK, 1, &num_tags_per_block
      , AMP_TYPE, 1, &amp_type
      , MAX_TIMES_PER_BLOCK, 1, &max_times_per_block
      , NUM_TIMES_TOTAL, 1, &num_times_total
      , NUM_CHANNELS, 1, &num_channels
      , NUM_STATIONS, 1, &num_stations
      );
    __CHECK

    if ((amp_type & 0x0F) != OSKAR_DOUBLE) {
      printf("Invalid data !\n");
      h = nullptr;
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

    num_blocks = (num_times_total + max_times_per_block - 1) /
    max_times_per_block;
    num_baselines = num_stations * (num_stations - 1) / 2;
    num_times_baselines = max_times_per_block * num_baselines;
    num_times_baselines_total = num_times_total * num_baselines;
    num_total = num_times_baselines_total * num_channels;

    /* Print header data. */
    printf("Max. number of times per block: %d\n", max_times_per_block);
    printf("Total number of times: %d\n", num_times_total);
    printf("Number of stations: %d\n", num_stations);
    return 0;
  }

  oskar_Binary* h;
  int
      amp_type
    , max_times_per_block
    , num_tags_per_block
    , num_baselines
    , num_channels
    , num_stations
    , num_times_total
    , num_blocks
    , num_times_baselines
    , num_times_baselines_total
    , num_total
    ;
  double
      phase_centre[2]
    , telescope_centre[3]
    , freq_start_inc[2]
    , time_start_inc[2]
    , channel_bandwidth_hz
    , time_average_sec
    ;
};


#define __CHECK1(s) if (status != 0) {printf("ERROR! at %s: %d\n", #s, status); return;}

static void read_reshuffle_and_write(const VisData & vd)
{
  int status = 0;

  double *u_temp, *v_temp, *w_temp, *uvws;
  double *amp_temp, *amps;
  double freq_start_inc[2], time_start_inc[2]; // local to block
  int dim_start_and_size[6];

  u_temp = (double *)calloc(vd.num_times_baselines, DBL_SZ);
  v_temp = (double *)calloc(vd.num_times_baselines, DBL_SZ);
  w_temp = (double *)calloc(vd.num_times_baselines, DBL_SZ);
  uvws = (double *)calloc(vd.num_times_baselines_total, 3 * DBL_SZ);
  amp_temp = (double *)calloc(vd.num_times_baselines * vd.num_channels, 8 * DBL_SZ);
  amps = (double *)calloc(vd.num_total, 8 * DBL_SZ);

  /* Loop over blocks and read each one. */
  for (int block = 0; block < vd.num_blocks; ++block)
  {
    int b, c, t, num_times, start_time_idx, start_channel_idx;

    /* Set search start index. */
    oskar_binary_set_query_search_start(vd.h, block * vd.num_tags_per_block, &status);

    /* Read block metadata. */
    status = bin_read_i(vd.h, vis_block_group, block
      , DIM_START_AND_SIZE, 6, dim_start_and_size
      );
    __CHECK1(DIM_START_AND_SIZE)

    status = bin_read_d(vd.h, vis_block_group, block
      , FREQ_REF_INC_HZ, 2, freq_start_inc
      , TIME_REF_INC_MJD_UTC, 2, time_start_inc
      );
    __CHECK1(FREQ_REF_INC_HZ + TIME_REF_INC_MJD_UTC)

      /* Get the number of times actually in the block. */
    start_time_idx = dim_start_and_size[0];
    start_channel_idx = dim_start_and_size[1];
    num_times = dim_start_and_size[2];

    /* Read the visibility data. */
    status = bin_read<OSKAR_DOUBLE_COMPLEX_MATRIX>(vd.h, vis_block_group, block
      , CROSS_CORRELATIONS, 4 * vd.num_times_baselines * vd.num_channels, amp_temp
      );
    __CHECK1(CROSS_CORRELATIONS)

    /* Read the baseline data. */
    status = bin_read_d(vd.h, vis_block_group, block
      , BASELINE_UU, vd.num_times_baselines, u_temp
      );
    __CHECK1(BASELINES)

    status = bin_read_d(vd.h, vis_block_group, block
      , BASELINE_VV, vd.num_times_baselines, v_temp
      );
    __CHECK1(BASELINES)

    status = bin_read_d(vd.h, vis_block_group, block
      , BASELINE_WW, vd.num_times_baselines, w_temp
      );
    __CHECK1(BASELINES)

    /* Check for errors. */
    if (status) break;

    /* Print contents of the block. */
    for (t = 0; t < num_times; ++t)
    {
      for (c = 0; c < vd.num_channels; ++c)
      {
        for (b = 0; b < vd.num_baselines; ++b)
        {
          int i, j;
          int tt, ct, it, jt, tmp;
          // Amplitudes are in row-major
          //  [timesteps][channels][baselines][polarizations] array
          // U, V and W are in row-major
          //  [timesteps][baselines] array
          // And we want to convert them to
          //  [baselines][timesteps][channels][polarizations]
          // and
          //  [baselines][timesteps]
          // correspondingly
          i = 8 * (b + vd.num_baselines * (c + vd.num_channels * t));
          j = b + vd.num_baselines * t;

          tt = start_time_idx + t;
          ct = start_channel_idx + c;
          tmp = vd.num_times_total * b + tt;

          jt = 3 * tmp;
          uvws[jt] = u_temp[j];
          uvws[jt + 1] = v_temp[j];
          uvws[jt + 2] = w_temp[j];

          it = (tmp * vd.num_channels + ct) * 8;
          for (int cc = 0; cc < 8; cc++) amps[it + cc] = amp_temp[i + cc];
        }
      }
    }
  }

  /* Close the file. */
  oskar_binary_free(vd.h);

  /* Print status message. */
  if (status != 0)
    printf("Failure reading test file.\n");
  else
    printf("Test file read successfully.\n");

  /* Free local arrays. */
  free(amp_temp);
  free(u_temp);
  free(v_temp);
  free(w_temp);

  FILE *ampf = fopen("amp.dat", "wb");
  fwrite(amps, 8 * sizeof(double), vd.num_total, ampf);
  fclose(ampf);
  free(amps);

  FILE *uvwf = fopen("uvw.dat", "wb");
  fwrite(uvws, 3 * sizeof(double), vd.num_times_baselines_total, uvwf);
  fclose(uvwf);
  free(uvws);
}

int main(int argc, const char * argv[])
{
  VisData vd;
  if (argc < 2) return -1;
  vd.mk_from_file(argv[1]);
  read_reshuffle_and_write(vd);
  return 0;
}
