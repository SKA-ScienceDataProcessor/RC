/* uvw2romein.c

  Example of o2a library usage.

  Reads and reshuffles u, v, w and amp data to the
  layout, suitable for Romein and Halide gridders consumption.

  Copyright (C) 2014 Braam Research, LLC.
 */

#include "o2a.h"

#include <stdio.h>

int main(int argc, char** argv){
  VisHandle vh;
  Dimensions dims;
  DoubleData data;
  int i, n;
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

      // No fancy optimizations. Plain stupid loop.
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
