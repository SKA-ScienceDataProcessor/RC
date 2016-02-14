#include <algorithm>

#include "grid_gpu.cuh"
#include "Defines.h"

template <typename T> inline
void bin_sort(T * in, int npts){
  // auto comparer = [npts] (T A, T B) {
  auto comparer = [npts] (const T & A, const T & B) {
    int blocksize = GCF_DIM/2;
    int mainxa = floor(A.x);
    int mainxb = floor(B.x);
    int mainya = floor(A.y);
    int mainyb = floor(B.y);
    int gridxa = mainxa/blocksize;
    int gridxb = mainxb/blocksize;
    int gridya = mainya/blocksize;
    int gridyb = mainyb/blocksize;
    if (gridya*(IMG_SIZE+blocksize-1)/blocksize+gridxa >
        gridyb*(IMG_SIZE+blocksize-1)/blocksize+gridxb) return 1;
    if (gridya*(IMG_SIZE+blocksize-1)/blocksize+gridxa <
        gridyb*(IMG_SIZE+blocksize-1)/blocksize+gridxb) return -1;
    double suba = GCF_GRID*(A.x-mainxa) + A.y-mainya;
    double subb = GCF_GRID*(B.x-mainxb) + B.y-mainyb;
    if (suba > subb) return 1;
    if (suba < subb) return -1;
    return  0;
  };
  std::sort(in, in+npts, comparer);
}

void bin_sort3(double3 * in, int npts){ bin_sort(in, npts); }
void bin_sort5(combined * in, int npts){ bin_sort(in, npts); }
