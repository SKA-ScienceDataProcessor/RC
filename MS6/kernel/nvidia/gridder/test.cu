#include <iostream>
#include "math.h"
#include "stdlib.h"

#include "grid_gpu.cuh"
#include "Defines.h"

void init_gcf(double2 *gcf, size_t size) {
  for (size_t sub_x=0; sub_x<GCF_GRID; sub_x++ )
   for (size_t sub_y=0; sub_y<GCF_GRID; sub_y++ )
    for(size_t x=0; x<size; x++)
     for(size_t y=0; y<size; y++) {
       double tmp = sin(6.28*x/size/GCF_GRID)*exp(-(1.0*x*x+1.0*y*y*sub_y)/size/size/2);
       gcf[size*size*(sub_x+sub_y*GCF_GRID)+x+y*size].x = tmp*sin(1.0*x*sub_x/(y+1))+0.4;
       gcf[size*size*(sub_x+sub_y*GCF_GRID)+x+y*size].y = tmp*cos(1.0*x*sub_x/(y+1))-0.2;
     }
}

#ifdef __COMBINED
#define A_IM a_im
#define A_RE a_re
#else
#define A_IM x
#define A_RE y
#endif

int main(void) {
   double2* out = (double2*) malloc(sizeof(double2)*(IMG_SIZE*IMG_SIZE+2*IMG_SIZE*GCF_DIM+2*GCF_DIM)*POLARIZATIONS);
#ifdef __COMBINED
   combined* in_c = (combined*) malloc(sizeof(combined)*NPOINTS);
#define in      in_c
#define in_vals in_c
#else
   double3* in = (double3*) malloc(sizeof(double3)*NPOINTS);
   double2* in_vals = (double2*) malloc(sizeof(double2)*NPOINTS*POLARIZATIONS);
#endif
   double2 *gcf = (double2*) malloc(64*GCF_DIM*GCF_DIM*sizeof(double2));
   init_gcf(gcf, GCF_DIM);
   srand(2541617);
   for(size_t n=0; n<NPOINTS; n++) {
      in[n].x = ((float)rand())/0x7fff*IMG_SIZE;
      in[n].y = ((float)rand())/0x7fff*IMG_SIZE;
      for (int p=0;p<POLARIZATIONS;p++) {
         in_vals[POLARIZATIONS*n+p].A_IM = ((float)rand())/0x7fff;
         in_vals[POLARIZATIONS*n+p].A_RE = ((float)rand())/0x7fff;
      }
   }
   for (int x=0;x<IMG_SIZE*GCF_DIM*POLARIZATIONS+GCF_DIM*POLARIZATIONS;x++) {
      out[x].x=0.0;
      out[x].y=0.0;
      out[x+(IMG_SIZE*IMG_SIZE+IMG_SIZE*GCF_DIM+GCF_DIM)*POLARIZATIONS].x = 0.0;
      out[x+(IMG_SIZE*IMG_SIZE+IMG_SIZE*GCF_DIM+GCF_DIM)*POLARIZATIONS].y = 0.0;
   }
   std::cout << "Sorting...\n";
#ifdef __COMBINED
   bin_sort5(in, NPOINTS);
   std::cout << "Computing on GPU...\n";
   gridGPUc(0.1, out
     ,in_c
#else
   bin_sort3(in, NPOINTS);
   std::cout << "Computing on GPU...\n";
   gridGPUs(0.1, out
     ,in
     ,in_vals
#endif
     ,NPOINTS,IMG_SIZE,gcf);
   free(out);out=0;
#ifdef __COMBINED
   free(in_c);in_c=0;
#else
   free(in);in=0;
   free(in_vals);in_vals=0;
#endif
   free(gcf);gcf=0;
}
