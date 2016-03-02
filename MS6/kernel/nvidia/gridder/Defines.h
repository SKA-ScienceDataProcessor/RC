#ifndef __DEFINES_H
#define __DEFINES_H

#include "../../cpu/gridding/cfg.h"

#define NPOINTS (num_baselines*num_times)
#define GCF_DIM 16
#define IMG_SIZE 2048
#define GCF_GRID OVER
//PTS and GCF_STRIPES affect only GATHER
#define PTS 1
#define GCF_STRIPES 4
#define POLARIZATIONS 1
//#define DEBUG1
#endif
