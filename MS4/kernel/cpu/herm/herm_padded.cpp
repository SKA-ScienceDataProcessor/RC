#include "herm_padded.h"

#ifndef MOVE_TO_TOP

// Very simple. Only for even sizes.
void herm_padded_inplace(complexd * data, int size, int pitch){
  int
      pad = pitch - size
      // the same as (size-1)*(pitch+1)
    , dataend = size * pitch - pad - 1
    , jmp = pad + size / 2
    ;
  complexd
      * pv0 = data
    , * pv1 = data + dataend;
    ;
  // An extra complexity due to a padding of the data
  for (int i = 0; i < size; i++) {
    for (int j = 0; j < size / 2; j++) {
      *pv0 += conj(*pv1);
      pv0++;
      pv1--;
    }
    pv0 += jmp;
    pv1 -= jmp;
  }
}

#else

// Very simple. Only for even sizes.
void herm_padded_inplace(complexd * data, int size, int pitch){
  int
      pad = pitch - size
      // the same as (size-1)*(pitch+1)
    , dataend = size * pitch - pad - 1;
    ;
  complexd
      * pv0 = data
    , * pv1 = data + dataend;
    ;
  // An extra complexity due to a padding of the data
  for (int i = 0; i < size / 2; i++) {
    for (int j = 0; j < size; j++) {
      *pv0 += conj(*pv1);
      pv0++;
      pv1--;
    }
    pv0 += pad;
    pv1 -= pad;
  }
}

#endif
