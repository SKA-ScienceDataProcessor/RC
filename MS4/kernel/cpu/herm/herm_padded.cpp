#include "herm_padded.h"

#ifdef __FULL_HERM
#define __(a) a
#else
#define __(a)
#endif

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
      __(complexd tmp;)
      __(tmp = *pv0;)
      *pv0 += conj(*pv1);
      __(*pv1 += conj(tmp);)
      pv0++;
      pv1--;
    }
    pv0 += pad;
    pv1 -= pad;
  }
}
