#include <algorithm>

#include "grid_gpu.cuh"
#include "Defines.h"

template <typename T> inline
void bin_sort(T * in, int npts){
  auto comparer = [npts] (const T & A, const T & B) {
    int
       blocksize = GCF_DIM/2
     , mainxa = floor(A.x)
     , mainxb = floor(B.x)
     , mainya = floor(A.y)
     , mainyb = floor(B.y)
     , gridxa = mainxa/blocksize
     , gridxb = mainxb/blocksize
     , gridya = mainya/blocksize
     , gridyb = mainyb/blocksize
     ; 
    if (gridya*(IMG_SIZE+blocksize-1)/blocksize+gridxa >
        gridyb*(IMG_SIZE+blocksize-1)/blocksize+gridxb) return false;
    if (gridya*(IMG_SIZE+blocksize-1)/blocksize+gridxa <
        gridyb*(IMG_SIZE+blocksize-1)/blocksize+gridxb) return true;
    double
        suba = GCF_GRID*(A.x-mainxa) + A.y-mainya
      , subb = GCF_GRID*(B.x-mainxb) + B.y-mainyb
      ;
    if (suba > subb) return false;
    return true;
  };
  std::sort(in, in+npts, comparer);
}

void bin_sort3(double3 * in, int npts){ bin_sort(in, npts); }
void bin_sort5(combined * in, int npts){ bin_sort(in, npts); }
