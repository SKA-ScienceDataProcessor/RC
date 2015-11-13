#ifndef __GCF_CFG_H
#define __GCF_CFG_H

#define GCF16

const int
#ifdef GCF32
    GCF_SIZE = 32
#else
    GCF_SIZE = 16
#endif
  , OVER = 8
  ;

const int
    num_baselines = 32131
  , num_times = 200
  ;

#endif
