#ifndef __HERM_PADDED_H
#define __HERM_PADDED_H

#include "common.h"

#ifdef __cplusplus
extern "C"
#endif
void herm_padded_inplace(complexd * data, int size, int pitch);

#endif
