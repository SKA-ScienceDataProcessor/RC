// Halide support for DNA project.
// Copyright (C) 2014 Braam Research, LLC.

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/time.h>

#include <static_image.h>
#include <image_io.h>

extern "C" Image<float> * halideFloatImage1D(int extent1) {
        return new Image<float>(extent1);
} /* halideFloatImage1D */

extern "C" Image<float> * halideFloatImage2D(int extent1, int extent2) {
        return new Image<float>(extent1, extent2);
} /* halideFloatImage2D */

extern "C" Image<float> * halideFloatImage3D(int extent1, int extent2, int extent3) {
        return new Image<float>(extent1, extent2, extent3);
} /* halideFloatImage3D */

extern "C" Image<float> * halideFloatImage4D(int extent1, int extent2, int extent3, int extent4) {
        return new Image<float>(extent1, extent2, extent3, extent4);
} /* halideFloatImage4D */
