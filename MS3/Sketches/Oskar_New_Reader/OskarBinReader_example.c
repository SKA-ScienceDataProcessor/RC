/* OskarBinReader_example.

  Copyright (C) 2015 Braam Research, LLC.
 */

#include <stdio.h>
#include <stdlib.h>
#include "OskarBinReader.h"

int main(int argc, const char * argv[])
{
  VisData * vdp;
  double
      * uvws
    , * amps
    ;
  FILE
      *ampf
    , *uvwf
    ;

  if (argc < 2) return -1;
  vdp = mkFromFile(argv[1]);

  if(vdp) {
    uvws = (double*)calloc(vdp->num_times_baselines, 3 * DBL_SZ);
    amps = (double*)calloc(vdp->num_points, 8 * DBL_SZ);

    if (readAndReshuffle(vdp, amps, uvws) == 0) {
      ampf = fopen("amp.dat", "wb");
      fwrite(amps, 8 * sizeof(double), vdp->num_points, ampf);

      uvwf = fopen("uvw.dat", "wb");
      fwrite(uvws, 3 * sizeof(double), vdp->num_times_baselines, uvwf);
      fclose(uvwf);
    }

    free(amps);
    free(uvws);

    freeVisData(vdp);
    deleteVisData(vdp);
  }
  return 0;
}
