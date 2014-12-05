/* o2a.h

  Oskar visibilites file data reading library.
  This is a relatively thin layer on top of Oskar API.

  We choose to isolate user from *any* of Oskar compile-time dependencies.
  Only link-time dependency remains -- we shall link client programs against liboskar.a

  Copyright (C) 2014 Braam Research, LLC.
 */

#ifndef __OSKAR_VIS_READER_H
#define __OSKAR_VIS_READER_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef SPEED_OF_LIGHT
#define SPEED_OF_LIGHT 299792458.0
#endif

typedef struct Float2_tag
{
  float x;
  float y;
} Float2;

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

typedef struct Float4c_tag {
  Float2 a;
  Float2 b;
  Float2 c;
  Float2 d;
} Float4c;

typedef struct Double4c_tag {
  Double2 a;
  Double2 b;
  Double2 c;
  Double2 d;
} Double4c;


typedef struct Dimensions_tag {
  int channels;
  int timesteps;
  int baselines;
  int polarizations;
} Dimensions;

typedef struct FloatData_tag {
  const float * u;
  const float * v;
  const float * w;
  const Float4c * amp;
} FloatData;

typedef struct DoubleData_tag {
  const double * u;
  const double * v;
  const double * w;
  const Double4c * amp;
} DoubleData;

// Macro to be C-compatible
#define __IS_VALID_DATA(d) (d.u != NULL && d.v != NULL && d.w != NULL && d.amp != NULL)


// Amplitudes are in row-first
//   [channels][timesteps][baselines][polarizations] array
// U, V and W are in row-first
//   [timesteps][baselines] array

enum PolarizationType {
    StokesI = 0
  , StokesQ = 1
  , StokesU = 2
  , StokesV = 3
  , XX      = 4
  , XY      = 5
  , YX      = 6
  , YY      = 7
};

struct Vis;
typedef struct Vis * VisHandle;

VisHandle vis_allocate_and_read(const char * filepath);
int vis_free(VisHandle vh);

Dimensions vis_get_dimensions(const VisHandle vh);

FloatData vis_get_floatdata(const VisHandle vh);
DoubleData vis_get_doubledata(const VisHandle vh);

// Misc queries
int vis_num_stations(const VisHandle vh);
double vis_freq_start_hz(const VisHandle vh);
double vis_freq_inc_hz(const VisHandle vh);

#ifdef __cplusplus
}

// Perhaps don't need this.

#define __MK_POL_FUN(pt, code)     \
template <typename T, typename T2> \
T mkPolAmp##pt (T2 amp) {          \
  T a;                             \
  code                             \
  return a;                        \
}

__MK_POL_FUN(StokesI,
  a.x = 0.5 * (amp.a.x + amp.d.x);
  a.y = 0.5 * (amp.a.y + amp.d.y);
)

__MK_POL_FUN(StokesQ,
  a.x = 0.5 * (amp.a.x - amp.d.x);
  a.y = 0.5 * (amp.a.y - amp.d.y);
)

__MK_POL_FUN(StokesU,
  a.x = 0.5 * (amp.b.x + amp.c.x);
  a.y = 0.5 * (amp.b.y + amp.c.y);
)

__MK_POL_FUN(StokesV,
  a.x = 0.5 * (amp.b.y - amp.c.y);
  a.y = -0.5 * (amp.b.x - amp.c.x);
)

__MK_POL_FUN(XX,
  a.x = amp.a.x;
  a.y = amp.a.y;
)

__MK_POL_FUN(XY,
  a.x = amp.b.x;
  a.y = amp.b.y;
)

__MK_POL_FUN(YX,
  a.x = amp.c.x;
  a.y = amp.c.y;
)

__MK_POL_FUN(YY,
  a.x = amp.d.x;
  a.y = amp.d.y;
)

#endif

#endif
