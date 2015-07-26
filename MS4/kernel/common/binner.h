#ifndef __BINNER_H
#define __BINNER_H

#include "common.h"
#include "metrix.h"

// We 
typedef struct binTable_tag {
  void * data; // this should be freed
  char * visTable[DIVIDERS][DIVIDERS];
  char * preTable[DIVIDERS][DIVIDERS];
  int    lenTable[DIVIDERS][DIVIDERS];
} binTable;

extern "C" {
int bin(binTable tbl[], int num_channels, int num_points, double scale, double wstep, complexd* amps, Double3 * uvws, int gcfsupps[], int grid_size);
int binm(binTable tbl[], int num_channels, int num_points, double scale, double wstep, complexd* amps, Double3 * uvws, int gcfsupps[], int grid_size);
}

#endif
