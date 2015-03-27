/* OskarBinReader_example.

  Copyright (C) 2015 Braam Research, LLC.
 */

#include "OskarBinReader.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

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
  VisData * vdp;
  FILE *f;
  char fname[1024];

  if (argc < 2) return -1;
  
  sprintf(fname, "%s.vis", argv[1]);
  vdp = mkFromFile(fname);

  if(vdp) {
    Metrix m;
    __MKDNAME
    my_mk_dir(fname, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

    double * uvws = (double*)calloc(vdp->num_points, 3 * DBL_SZ);
    double * amps = (double*)calloc(vdp->num_points, 8 * DBL_SZ);
    WMaxMin * bl_ws = (WMaxMin*)calloc(vdp->num_baselines, sizeof(WMaxMin));
    BlWMap * bl_wis = (BlWMap *)calloc(vdp->num_baselines, sizeof(BlWMap));

    printf("Started to read and reshuffle ...\n");
    if (readAndReshuffle(vdp, amps, uvws, &m, bl_ws, bl_wis) == 0) {
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

      printf("Done. writing baseline map ...\n");
      __MKF("bl_wis.dat")
      fwrite(bl_wis, sizeof(int), vdp->num_baselines, f);
      fclose(f);
      
      printf("Done. writing metrix ...\n");
      __MKF("metrix.dat")
      fwrite(&m, sizeof(Metrix), 1, f);
      fclose(f);

#ifdef __MAKE_TEXT_REPORTS
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
      for (int i = 0; i < vdp->num_baselines; i++)
        fprintf(f, "%8d: %f %f\n", i, bl_ws[i].minw, bl_ws[i].maxw);
      fclose(f);

      printf("Done. printing baselines map ...\n");
      __MKF("bl_wis.txt")
      for (int i = 0; i < vdp->num_baselines; i++)
        fprintf(f, "%8d: %d\n", bl_wis[i].bl, bl_wis[i].wp);
      fclose(f);

#endif
    }

    free(amps);
    free(uvws);
    free(bl_ws);
    free(bl_wis);

    freeVisData(vdp);
    deleteVisData(vdp);
  }
  return 0;
}
