/* OskarBinReader.h

  Oskar 2.6.x binary file data reader.

  Reads and reshuffles amp and uvw data to the
  layout, suitable for Romein-like gridder consumption:

  Amplitudes are in row-major
     [baselines][timesteps][channels][polarizations].
  U, V and W are in row-major
     [baselines][timesteps][uvw].

  Copyright (C) 2015 Braam Research, LLC.
 */

#ifndef __OSKAR_BIN_READER_H
#define __OSKAR_BIN_READER_H

typedef struct BlWMap_tag {
  int
      bl
    , wp
    ;
} BlWMap;

#ifndef __CUDACC__

#define DBL_SZ sizeof(double)

#ifndef OSKAR_BINARY_H_
struct oskar_Binary;
typedef struct oskar_Binary oskar_Binary;
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct VisData_tag {
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
} VisData;


typedef struct Metrix_tag {
  double
      maxu
    , maxv
    , maxw
    , minu
    , minv
    , minw
    , wstep
    ;
} Metrix;

// Calculate for each baseline
typedef struct WMaxMin_tag {
  double
      maxw
    , minw
    ;
} WMaxMin;


VisData * mkFromFile(const char * filename);
void freeVisData(VisData * vdp);
void deleteVisData(VisData * vdp);

int readAndReshuffle(const VisData * vdp, double * amps, double * uvws, Metrix * mp, WMaxMin * bl_ws, BlWMap * bl_wis);


#ifdef __cplusplus
}
#endif

#endif

#endif
