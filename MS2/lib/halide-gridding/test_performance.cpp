// performance testing for gridding.
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/time.h>

extern "C" {
  #include "griddingSimple_float.h"
}

#include <static_image.h>
#include <image_io.h>

static double get_s(void) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (double)tv.tv_sec + 1.0e-6*tv.tv_usec;
} /* get_s */

static void testGriddingSimpleConformance(void) {
    int nBaselines = 10;
    int nTimesteps = 100;
    int maxSupportSize = 101;
    int resultWidthHeight = 2048;
    int bl, ts, i, j;
    Image<float> UVWTriples(nBaselines, nTimesteps, 3);
    Image<float> visibilities(nBaselines, nTimesteps, 8);
    Image<float> support(nBaselines, maxSupportSize, maxSupportSize, 2);
    Image<int> supportSize(nBaselines, 1);

    // looping over baselines and set support sizes, etc.
    for (bl = 0; bl < nBaselines; bl++) {
        // setting up support.
        int supportSizeForBL = maxSupportSize;
        supportSize(bl) = supportSizeForBL;
        for (i=0;i<supportSizeForBL; i++) {
           for (j=0;j<supportSizeForBL; j++) {
               float value = 0.0;       // values are zeroes except
               if (i==j || i == (supportSizeForBL-j-1)) // both diagonals are ones.
                   value = 1.0;
               support(bl, i, j, 0) = value;
               support(bl, i, j, 0) = -value;
           }
        }

        // Setting up UVW triples and visibilities.
        for (ts = 0; ts < nTimesteps; ts ++) {
            // Setting up UVW.
            // U and V are set as pseudorandom to cover all the image.
            int U = (bl*131+ts*61) % resultWidthHeight;
            int V = (bl*17+ts*97) % resultWidthHeight;
            UVWTriples(bl, ts, 0) = (float)U;
            UVWTriples(bl, ts, 1) = (float)V;
            UVWTriples(bl, ts, 2) = (float)11;

            // Setting up visibilities.
            for (i=0;i<8;i++) {
                int vis = (bl*101+ts*313 + i*503) % 1000;
                visibilities(bl, ts, 0) = ((float)vis)/1000.0;
            }
        }
    }

    // execute the algorithm.
    Image<float> result(resultWidthHeight, resultWidthHeight, 4, 2);

    printf("Starting.\n");
    double runtime = 0;
    runtime -= get_s();
    int errCode = griddingSimple_float(UVWTriples, visibilities, support, supportSize, result);
    runtime += get_s();
    double flopCount = nBaselines;      // we run across all baselines.
    flopCount *= nTimesteps;            // and also across all timesteps.
    flopCount *= maxSupportSize*maxSupportSize; // for each baseline, timestep pair we perform gridding with square support 
    flopCount *= 8*2;   // 4 polarizations, each is complex. We multiply and add.
    printf("execution error code %d.\n",errCode);
    printf("time to execute %lf s.\n", runtime);
    printf("FLOPS %lf.\n", flopCount/runtime);
} /* testGriddingSimpleConformance */


int main(int argc, char **argv) {

    testGriddingSimpleConformance();

    return 0;
}
