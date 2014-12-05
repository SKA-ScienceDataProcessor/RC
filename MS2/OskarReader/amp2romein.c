/* amp2romein.c

  Example of o2a library usage.

  Reads and reshuffles amp data to the
  layout, suitable for Romein (R) and Halide (H) gridders consumption.

  Copyright (C) 2014 Braam Research, LLC.
 */

#include "o2a.h"

#include <stdio.h>

int main(int argc, char** argv){
  VisHandle vh;
  Dimensions dims;
  DoubleData data;
  int
      ch, ts, bl
    , CH, TS, BL
    , CSTEP
    ;
  FILE * out;
  const Double4c *camp;

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
      printf("Writing amp ...\n");
      out = fopen("amp.dat", "wb");

      // To be compatible with H/R shall swap
      // baselines, timesteps and channels dimensions.
      // No fancy optimizations. Plain stupid loops.
      // [channels][timesteps][baselines][polarizations] ->
      //   -> [baselines][timesteps][channels][polarizations]
      BL = dims.baselines;
      TS = dims.timesteps;
      CH = dims.channels;
      CSTEP = TS*BL;
      camp = data.amp;
      for ( bl = 0; bl < BL; bl++ ) {
        for ( ts = 0; ts < TS; ts++ ) {
          for ( ch = 0; ch < CH; ch++ ) {
            fwrite(camp + CSTEP * ch + BL * ts + bl, sizeof (*camp), 1, out);
          }
        }
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
