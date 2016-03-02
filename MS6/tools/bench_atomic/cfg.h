#ifndef __GCF_CFG_H
#define __GCF_CFG_H

#define GCF16

#ifdef GCF32
#define GCF_SIZE 32
#else
#define GCF_SIZE 16
#endif
#define OVER 8

const int
    num_baselines = 32131
  , num_times = 200
  ;

#endif
