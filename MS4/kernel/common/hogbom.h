#ifndef __HOGBOM_H
#define __HOGBOM_H

#ifdef __cplusplus
extern "C"
#endif
void deconvolve(
    double * mod_p
  , double * res_p
  , double * psf_p // not a 'const' because modifies the pad area (zeroes)
  , int siz
  , int pitch
  , unsigned int niters
  , double gain
  , double threshold
  );

#endif
