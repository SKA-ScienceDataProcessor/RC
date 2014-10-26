#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/time.h>

extern "C" {
  #include "griddingSimple_float.h"
}

#include <static_image.h>
#include <image_io.h>

static void testGriddingSimpleConformance(void) {
    int nBaselines = 2;
    int nChannels = 1;
    int nTimesteps = 1;
    Image<float> UVWTriples(nBaselines, nChannels, nTimesteps, 3);
    Image<float> visibilities(nBaselines, nChannels, nTimesteps, 8);
    Image<float> support(nBaselines, 3, 3, 2);
    Image<int> supportSize(nBaselines, 1);

    // setting up UVW triples.
    // center is at (5,6), width 1.
    UVWTriples(0,0,0,0) =   5.1; UVWTriples(0,0,0,1) =   6.1; UVWTriples(0,0,0,2) =   7.2;
    UVWTriples(1,0,0,0) =  12.1; UVWTriples(1,0,0,1) =  13.1; UVWTriples(1,0,0,2) =  14.2;

    // setting up visibilities.
    // the last dimension is complex pair * polarization.
    visibilities(0,0,0,0) = 0.1; visibilities(0,0,0,1) = 0.2; visibilities(0,0,0,2) = 0.3; visibilities(0,0,0,3) = 0.4;
    visibilities(0,0,0,5) = 0.5; visibilities(0,0,0,6) = 0.6; visibilities(0,0,0,7) = 0.7; visibilities(0,0,0,8) = 0.8;
    visibilities(1,0,0,0) = 0.1; visibilities(1,0,0,1) = 0.2; visibilities(1,0,0,2) = 0.3; visibilities(1,0,0,3) = 0.4;
    visibilities(1,0,0,5) = 0.5; visibilities(1,0,0,6) = 0.6; visibilities(1,0,0,7) = 0.7; visibilities(1,0,0,8) = 0.8;

    // setting up support sizes:
    supportSize(0) = 1;
    supportSize(1) = 3;

    // Setting up support.
    // baseline 0 - size 1, single element should be set.
    support(0,0,0,0) = 0.5; support(0,0,0,1) = -1;

    support(1,0,0,0) = 1; support(0,0,0,1) = -1;
    support(1,0,1,0) = 2; support(0,0,0,1) = -2;
    support(1,0,2,0) = 3; support(0,0,0,1) = -3;
    support(1,1,0,0) = 4; support(0,1,0,1) = -4;
    support(1,1,1,0) = 5; support(0,1,1,1) = -5;
    support(1,1,2,0) = 6; support(0,1,2,1) = -6;
    support(1,2,0,0) = 7; support(0,2,0,1) = -7;
    support(1,2,1,0) = 8; support(0,2,1,1) = -8;
    support(1,2,2,0) = 9; support(0,2,2,1) = -9;

    // execute the algorithm.
    Image<float> result(20, 20, 4, 2);

    int errCode = griddingSimple_float(UVWTriples, visibilities, support, supportSize, result);
    printf("execution error code %d.\n",errCode);
    if (errCode == 0) {
        int i,j,k;
        printf("Result:\n");
        for (i=0;i<result.extent(0);i++) {
            for (j=0;j<result.extent(1);j++) {
                for (k=0;k<result.extent(2);k++) {
                    bool correct = false;
                    if (i == 5 && j == 6) {
                       correct = result(i,j,k,0) == visibilities(0,0,0,k*2)*support(0,0,0,0)
                               && result(i,j,k,1) == visibilities(0,0,0,k*2+1)*support(0,0,0,1);
                    } else {
                       correct = result(i,j,k,0) == 0 && result(i,j,k,1) == 0;
                    }
                    if (!correct) {
                        printf("    incorrect result(%3d, %3d, %3d) = (%f, %f)\n", i, j, k, result(i,j,k,0), result(i,j,k,1));
                    }
                }
            }
        }
    }
} /* testGriddingSimpleConformance */


int main(int argc, char **argv) {

    testGriddingSimpleConformance();

    return 0;
}
