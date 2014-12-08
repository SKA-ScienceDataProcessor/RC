#include <static_image.h>

#include "griddingSimple_double_CUDA.h"

#ifndef BASELINES
#define BASELINES (44*(44-1)/2)
#endif
#ifndef TIMESTEPS
#define TIMESTEPS 20
#endif
#ifndef BLOCKS
#define BLOCKS 108
#endif
#ifndef SUPPORT_U
#define SUPPORT_U 96
#endif
#ifndef GRID_U
#define GRID_U 2048
#endif

extern "C"
int performHalideGriddingDouble(const double * uvw, const double * amp, Image<double> ** resp) {
    int nBaselines = BASELINES;
    int nTimesteps = TIMESTEPS * BLOCKS;
    int maxSupportSize = SUPPORT_U;
    int resultWidthHeight = GRID_U;
    int bl, i, j;

    Image<double> support(nBaselines, maxSupportSize, maxSupportSize, 2);
    Image<int> supportSize(nBaselines);

    buffer_t UVWTriples = {
        0                            // dev
      , const_cast<uint8_t*>(reinterpret_cast<const uint8_t*>(uvw))
      , {nBaselines, nTimesteps, 3}  // extents
      , {1, nBaselines * nTimesteps} // strides
      , {0}                          // minimums
      , sizeof(double)
      , true                         // host_dirty, can by any, because no dev is still involved
      , false                        // dev_dirty
      };
    
    buffer_t visibilities = {
        0
      , const_cast<uint8_t*>(reinterpret_cast<const uint8_t*>(amp))
      , {nBaselines, nTimesteps, 8}
      , {1, nBaselines * nTimesteps}
      , {0}
      , sizeof(double)
      , true
      , false
      };
    
    // looping over baselines and set support sizes, etc.
    for (bl = 0; bl < nBaselines; bl++) {
        // setting up support.
        int supportSizeForBL = maxSupportSize;
        supportSize(bl) = supportSizeForBL;
        for (i=0;i<supportSizeForBL; i++) {
           for (j=0;j<supportSizeForBL; j++) {
               double value = 0.0;       // values are zeroes except
               if (i==j || i == (supportSizeForBL-j-1)) // both diagonals are ones.
                   value = 1.0;
               support(bl, i, j, 0) = value;
               support(bl, i, j, 0) = -value;
           }
        }
    }

    // execute the algorithm.
    Image<double> * res = new Image<double>(resultWidthHeight, resultWidthHeight, 4, 2);
    *resp = res;
    int errcode = griddingSimple_double_CUDA(&UVWTriples, &visibilities, support, supportSize, *res);
    res->copy_to_host();
    return errcode;
}

extern "C" void finalizeDoubleImage(Image<double> * im) {
  delete im;
}
