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

/*
double round_out(double v){
  if (v < 0.0) return -round(-v);
  else return v;
} */

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

    double * uvws = (double*)calloc(vdp->num_times_baselines, 3 * DBL_SZ);
    double * amps = (double*)calloc(vdp->num_points, 8 * DBL_SZ);
    WMaxMin * bl_ws = (WMaxMin*)calloc(vdp->num_times_baselines, sizeof(WMaxMin));
    int * bl_wis = (int*)calloc(vdp->num_times_baselines, sizeof(int));

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

      printf("Done. computing baseline map ...\n");
      double
          maxxw = max(m.maxw, -m.minw)
        , wstep = maxxw/32 // 65 planes total
        , cmax, cmin
        ;

      for (int i = 0; i < vdp->num_baselines; i++) {
        cmax = bl_ws[i].maxw/wstep;
        cmin = bl_ws[i].minw/wstep;
        if (cmax > 0.0)
          bl_wis[i] = round(cmax);
        else if (cmin < 0.0)
          bl_wis[i] = -round(-cmin);
        else
          bl_wis[i] = 0;
      }
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
        fprintf(f, "%8d: %d\n", i, bl_wis[i]);
      fclose(f);

#endif
    }
    // We interpret WPLANES a bit differently here as being 65/2.
    // We center wplanes at 0.
    

    free(amps);
    free(uvws);
    free(bl_ws);

    freeVisData(vdp);
    deleteVisData(vdp);
  }
  return 0;
}
