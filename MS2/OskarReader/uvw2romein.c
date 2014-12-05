/* uvw2romein.c

  Example of o2a library usage.

  Reads and reshuffles u, v, w data to the
  layout, suitable for Romein (R) and Halide (H) gridders consumption.

  Copyright (C) 2014 Braam Research, LLC.
 */

#include "o2a.h"

#include <stdio.h>

int main(int argc, char** argv){
  VisHandle vh;
  Dimensions dims;
  DoubleData data;
  #ifdef __OSKAR_NATIVE_ORDER
  int i;
  int n;
  #else
  int bl, ts;
  int BL, bloff;
  #endif
  FILE * out;
  const double *cu, *cv, *cw;

  if (argc < 2) return -1;

  printf("Start opening %s\n", argv[1]);

  vh = vis_allocate_and_read(argv[1]);

  if (vh == NULL) {
    printf("Can't open %s\n", argv[1]);
    return -2;
  } else {
    data = vis_get_doubledata(vh);
    if (__IS_VALID_DATA(data)) {
      dims = vis_get_dimensions(vh);
      printf("Writing u v w ...\n");
      out = fopen("uvw.dat", "wb");

      #ifdef __OSKAR_NATIVE_ORDER
      n = dims.timesteps * dims.baselines;
      for ( i = 0
          , cu = data.u
          , cv = data.v
          , cw = data.w
          ; i < n
          ; i++, cu++, cv++, cw++
          ) {
        fwrite(cu, sizeof (*cu), 1, out);
        fwrite(cv, sizeof (*cv), 1, out);
        fwrite(cw, sizeof (*cw), 1, out);
      }
      #else
      // To be compatible with H/R shall swap
      // baselines and timesteps dimensions.
      // No fancy optimizations. Plain stupid loops.
      cu = data.u;
      cv = data.v;
      cw = data.w;
      BL = dims.baselines;
      for ( bl = 0; bl < BL; bl++ ) {
        for ( ts = 0; ts < dims.timesteps; ts++ ) {
          bloff = BL * ts + bl;
          fwrite(cu + bloff, sizeof (*cu), 1, out);
          fwrite(cv + bloff, sizeof (*cv), 1, out);
          fwrite(cw + bloff, sizeof (*cw), 1, out);
        }
      }
      #endif

      fclose(out);
    } else {
      printf("Can't allocate datastructures.\n");
    }

    printf("Freeing %s\n", argv[1]);
    vis_free(vh);
    printf("Done");
    return 0;
  }
}
