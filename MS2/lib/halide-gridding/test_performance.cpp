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

static inline int max(int a, int b) { return a < b ? b : a; }
static inline int min(int a, int b) { return a > b ? b : a; }

static void simpleGriddingLoop(Image<float> &UVWTriples,
    Image<float> &visibilities, Image<float> &support,
    Image<int> &supportSize, Image<float> &result) {
    int bl, ts;
    int u, v;
    int uStep = 8, vStep = 32;
    float *resultBuffer = &result(0,0,0,0);
    for (u = 0; u < result.extent(0); u++) {
        for(v = 0; v < result.extent(1); v++) {
            int pol;
            for (pol = 0; pol < 4; pol++) {
                result(u,v,pol,0) = 0.0;
                result(u,v,pol,1) = 0.0;
            }
        }
    }
    // cutting image into parts that fit into cache.
    for (u = 0; u<result.extent(0); u+= uStep) {
        for (v = 0; v < result.extent(1); v+= vStep) {
            // then for each that part we walk along all points.
            for (bl = 0; bl < UVWTriples.extent(0); bl++) {
               int widthHalf = supportSize(bl)/2;
               for (ts = 0; ts < UVWTriples.extent(1); ts++) {
                   int U = UVWTriples(bl, ts, 0);
                   if (U + widthHalf < u || U-widthHalf >= u+uStep)
                       continue;
                   int V = UVWTriples(bl, ts, 1);
                   if (V + widthHalf < v || V-widthHalf >= v+vStep)
                       continue;
                   int uInner, vInner;
                   for (uInner = max(u,U-widthHalf); uInner <= min(u+uStep-1, U+widthHalf); uInner++) {
                       for (vInner = max(v,V-widthHalf); vInner <= min(v+vStep-1, V+widthHalf); vInner++) {
                           int x = uInner - (U-widthHalf);
                           int y = vInner - (V-widthHalf);
                           float supR = support(bl, x, y, 0);
                           float supI = support(bl, x, y, 1);
                           int pol;
                           for (pol=0;pol<4;pol++) {
                               result(uInner, vInner, pol, 0) += supR * visibilities(bl, ts, pol*2) ;
                               result(uInner, vInner, pol, 1) += supR * visibilities(bl, ts, pol*2+1) ;
                           }
                       }
                   }
               }
            }
        }
    }
} /* simpleGriddingLoop */

static bool resultsEqual(Image<float> &result1, Image<float> &result2) {
    int diffCount = 0;
    for (int u=0;u<result1.extent(0);u++) {
        for (int v=0;v<result1.extent(1);v++) {
            for (int pol=0;pol<result1.extent(2);pol++) {
                for (int i=0;i<result1.extent(3);i++) {
                    float a = result1(u,v,pol,i);
                    float b = result2(u,v,pol,i);
                    float diff = abs(a - b);
                    float m = abs(a) < abs(b) ? abs(b) : abs(a);
                    bool neq = diff > m*0.000000001;
                    if (neq) {
                        printf("difference at (%d, %d, %d, %d), %f != %f.\n", u, v, pol, i, a, b);
                        diffCount ++;
                        if (diffCount > 10)
                            return false;
                    }
                }
            }
        }
    }
    return diffCount == 0;
} /* resultsEqual */


static void testGriddingSimpleConformance(void) {
    int nBaselines = 40;
    int nTimesteps = 40;
    int maxSupportSize = 101;
    int resultWidthHeight = 2048;
    int bl, ts, i, j;
    Image<float> UVWTriples(nBaselines, nTimesteps, 3);
    Image<float> visibilities(nBaselines, nTimesteps, 8);
    Image<float> support(nBaselines, maxSupportSize, maxSupportSize, 2);
    Image<int> supportSize(nBaselines);

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
    double flopCount = nBaselines;      // we run across all baselines.
    flopCount *= nTimesteps;            // and also across all timesteps.
    flopCount *= maxSupportSize*maxSupportSize; // for each baseline, timestep pair we perform gridding with square support 
    flopCount *= 8*2;   // 4 polarizations, each is complex. We multiply and add.
    runtime -= get_s();
    int errCode = griddingSimple_float(UVWTriples, visibilities, support, supportSize, result);
    runtime += get_s();
    printf("execution error code %d.\n",errCode);
    printf("time to execute %lf s.\n", runtime);
    printf("FLOPS %lf.\n", flopCount/runtime);
    runtime = 0;
    Image<float> result2(resultWidthHeight, resultWidthHeight, 4, 2);
    runtime -= get_s();
    simpleGriddingLoop(UVWTriples, visibilities, support, supportSize, result2);
    runtime += get_s();
    if (!resultsEqual(result, result2))
        printf("results are different, all bets are off.\n");
    printf("time to execute hand made loop %lf s.\n", runtime);
    printf("FLOPS %lf.\n", flopCount/runtime);
} /* testGriddingSimpleConformance */


int main(int argc, char **argv) {

    testGriddingSimpleConformance();

    return 0;
}
