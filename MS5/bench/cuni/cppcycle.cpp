#include <vector>
#include <complex>
#include <fstream>
#include <cstring>

#include "scatter_gridder_fixed_gcf_5t_vis.h"

using namespace std;

typedef complex<double> complexd;

// Config
const double t2 = 0.2/2.0;
const int pad = 0;
const int over2 = over*over;
const int gridSize = 8192;
const int gridPad = 0;

const int gridPitch = gridSize + gridPad;
const int fullSize = gridPitch * gridSize;

const int numOfVis = num_baselines * num_times;
const int numOfDoubles = numOfVis * 5;

constexpr int gcfStorageSize(int gcfSize){
  return over * gcfSize * (over * gcfSize + pad);
}

#if 0
template <typename T>
void writeImgToDisk(const char * fname, T * out){
  FILE * f = fopen(fname, "wb");
  if (f == NULL) {
    printf("Can't open %s\n", fname);
    return;
  }
  printf("Writing %s ...\n", fname);
  for (int r = 0; r < gridSize; r++, out += gridPitch)
    fwrite(out, sizeof(T), gridSize, f);
  fclose(f);
}

// Normalization is done inplace!
inline void normalizeCPU(
    complexd src[]
  , int grid_pitch
  , int grid_size
  )
{
  int siz = grid_size*grid_pitch;
  double norm = 1.0/double(siz);
#pragma omp parallel for
  for (int i = 0; i < siz; i++) {
    src[i] *= norm;
  }
}
#endif

typedef vector<double> vecd; 

// v should be preallocated with right size
int readFileToVector(vecd & v, const char * fname){
  ifstream is(fname, ios::binary);
  if (is.fail()) {
    printf("Can't open %s.\n", fname);
    return -1;
  }
  is.read(reinterpret_cast<char*>(v.data()), v.size() * sizeof(double));
  if (is.fail()) {
    printf("Can't read %s.\n", fname);
    return -2;
  }
  return 0;
}

template <bool chunked, int gcf_size, kern_type<chunked, gcf_size> kern>
void bench(
    const vecd & vis
  , const vecd & gcf
  , vecd & uvg
  ){
  memset(uvg.data(), 0, fullSize * 2 * sizeof(double));

  printf("%s, gcf size %d:\n", chunked ? "Chunked" : "Linear", gcf_size);
  typedef const complexd (*gcf_t)[over][over][gcf_size][gcf_size];
  for(int rep = 0; rep < 4; rep++) {
    kern(
        t2
      , reinterpret_cast<complexd*>(uvg.data())
      , *reinterpret_cast<gcf_t>(gcf.data())
      , *reinterpret_cast<typename dlayout<chunked>::type*>(vis.data())
      , gridSize
      );
  }
}

#define __CK if (res < 0) return res;

int main(/* int argc, char * argv[] */)
{
  int res;

  vecd vis(numOfDoubles);
  printf("Read visibilities!\n");
  res = readFileToVector(vis, "vis.dat"); __CK

  vecd gcf16(gcfStorageSize(16) * 2); // complex
  printf("Read GCF16!\n");
  res = readFileToVector(gcf16, "gcf16.dat"); __CK

  vecd gcf32(gcfStorageSize(32) * 2); // complex
  printf("Read GCF32!\n");
  res = readFileToVector(gcf32, "gcf32.dat"); __CK

  vecd uvg(fullSize * 2); // complex

  bench<true, 16, gridKernel_scatter_full_chunked16>(vis, gcf16, uvg);
  bench<false, 16, gridKernel_scatter_full16>(vis, gcf16, uvg);
  bench<true, 32, gridKernel_scatter_full_chunked32>(vis, gcf32, uvg);
  bench<false, 32, gridKernel_scatter_full32>(vis, gcf32, uvg);
	
#if 0
  normalizeCPU(
      reinterpret_cast<complexd*>(uvg.data())
    , gridPitch
    , gridSize
    );

  printf("Write!\n");
  writeImgToDisk("grid.dat", reinterpret_cast<complexd*>(uvg.data()));
#endif
  return 0;
}
