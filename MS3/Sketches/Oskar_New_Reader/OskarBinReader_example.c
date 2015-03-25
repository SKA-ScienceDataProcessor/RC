/* OskarBinReader_example.

  Copyright (C) 2015 Braam Research, LLC.
 */

#include "OskarBinReader.h"

#include <stdio.h>
#include <stdlib.h>

// directory stuff
#ifdef _WIN32
#include <direct.h>
#define my_mk_dir(name, mode) mkdir(name)
#else
#include <sys/stat.h>
#define my_mk_dir(name, mode) mkdir(name, mode)
#endif

#define __PREFIX "_"
#define __MKDNAME sprintf(fname, "%s%s", __PREFIX, argv[1]);
#define __MKF(n) sprintf(fname, "%s%s/%s", __PREFIX, argv[1], n); f = fopen(fname, "wb");

int main(int argc, const char * argv[])
{
  int i;
  VisData * vdp;
  double
      * uvws
    , * amps
    ;
  WMaxMin * bl_ws;
  FILE *f;
  char fname[1024];

  if (argc < 2) return -1;
  
  sprintf(fname, "%s.vis", argv[1]);
  vdp = mkFromFile(fname);

  if(vdp) {
    __MKDNAME
    my_mk_dir(fname, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

    uvws = (double*)calloc(vdp->num_times_baselines, 3 * DBL_SZ);
    amps = (double*)calloc(vdp->num_points, 8 * DBL_SZ);
    bl_ws = (WMaxMin*)calloc(vdp->num_times_baselines, sizeof(WMaxMin));
    Metrix m;

    printf("Started to read and reshuffle ...\n");
    if (readAndReshuffle(vdp, amps, uvws, &m, bl_ws) == 0) {
      printf("Done. writing amps ...\n");
      __MKF("amp.dat")
      fwrite(amps, 8 * sizeof(double), vdp->num_points, f);
      fclose(f);

      printf("Done. writing uvws ...\n");
      __MKF("uvw.dat")
      fwrite(uvws, 3 * sizeof(double), vdp->num_times_baselines, f);
      fclose(f);

      printf("Done. writing baseline stats ...\n");
      __MKF("bl_ws.dat")
      fwrite(bl_ws, sizeof(WMaxMin), vdp->num_baselines, f);
      fclose(f);

      printf("Done. writing metrix ...\n");
      __MKF("metrix.dat")
      fwrite(&m, sizeof(Metrix), 1, f);
      fclose(f);

      printf("Done. printing metrix report ...\n");
      __MKF("metrix.txt")
      fprintf(f,
        "maxu = %f\n"
        "maxv = %f\n"
        "maxw = %f\n"
        "minu = %f\n"
        "minv = %f\n"
        "minw = %f\n"
       , m.maxu
       , m.maxv
       , m.maxw
       , m.minu
       , m.minv
       , m.minw
      );
      fclose(f);

      printf("Done. printing baselines report ...\n");
      __MKF("bl_ws.txt")
      for (i = 0; i < vdp->num_baselines; i++)
        fprintf(f, "%8d: %f %f\n", i, bl_ws[i].minw, bl_ws[i].maxw);
      fclose(f);
    }

    free(amps);
    free(uvws);
    free(bl_ws);

    freeVisData(vdp);
    deleteVisData(vdp);
  }
  return 0;
}
