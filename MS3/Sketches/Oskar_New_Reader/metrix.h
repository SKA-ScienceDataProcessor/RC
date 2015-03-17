#ifndef GRID_U
#define GRID_D 2048
#else
#define GRID_D GRID_U
#endif
#ifndef SUPPORT_U
#define SUPP_D 32
#else
#define SUPP_D SUPPORT_V
#endif
#ifndef DIVIDERS
#define DIVIDERS 16
#endif
#ifndef WPLANES
#define WPLANES 32
#endif
#ifndef OVER
#define OVER 8
#endif

#define BSTEP (GRID_D/DIVIDERS)
#define SUPP_H (SUPP_D/2)

#define NUM_POL 4

struct vis_data {
  int u;
  int v;
  int wplane;
  short fracu;
  short fracv;
#ifdef __CUDACC__
  cuDoubleComplex XX;
  cuDoubleComplex XY;
  cuDoubleComplex YX;
  cuDoubleComplex YY;
#else
  Double4c vis;
#endif
};

struct vis_data_raw
{
  double u;
  double v;
  double w;
#ifdef __CUDACC__
  cuDoubleComplex XX;
  cuDoubleComplex XY;
  cuDoubleComplex YX;
  cuDoubleComplex YY;
#else
  Double4c vis;
#endif
};

#ifdef __NO_PREGRID
typedef vis_data_raw vis;
#else
typedef vis_data vis;
#endif
