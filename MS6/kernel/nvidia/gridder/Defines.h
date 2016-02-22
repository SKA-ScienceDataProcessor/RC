#ifndef __DEFINES_H
#define __DEFINES_H

#include "../../cpu/gridding/cfg.h"

#define NPOINTS (num_baselines*num_times)
#define GCF_DIM GCF_SIZE
#define IMG_SIZE 8192
#define GCF_GRID OVER
//PTS and GCF_STRIPES affect only GATHER
#define PTS 2
#define GCF_STRIPES 4
#define POLARIZATIONS 1
//#define DEBUG1
#endif
