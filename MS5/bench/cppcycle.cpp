#include <vector>
#include <complex>
#include <fstream>
#include <ctime>
#include <cstring>

#include "kernels_halide.h"
#include "mkHalideBuf.h"
#include "cfg.h"

#ifdef GCF32
#define GCF_FILE "gcf32.dat"
#else
#define GCF_FILE "gcf16.dat"
#endif

using namespace std;

typedef complex<double> complexd;

// Config
const double t2 = 0.2/2.0;
const int over = OVER;
const int pad = 0;
const int over2 = over*over;
const int gcfSize = GCF_SIZE;
const int gcfStorageSize = over * gcfSize * (over * gcfSize + pad);
const int gridSize = 8192;
const int gridPad = 0;

const int gridPitch = gridSize + gridPad;
const int fullSize = gridPitch * gridSize;

const int numOfVis = num_baselines * num_times;
const int numOfDoubles = numOfVis * 5;

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

#define __CK if (res < 0) return res;

int main(/* int argc, char * argv[] */)
{
  int res;
  vecd vis(numOfDoubles);
  printf("Read visibilities!\n");
  res = readFileToVector(vis, "vis.dat"); __CK
  vecd gcf(gcfStorageSize * 2); // complex
  printf("Read GCF!\n");
  res = readFileToVector(gcf, GCF_FILE); __CK

  vecd uvg(fullSize * 2); // complex

  buffer_t
      vis_buffer = mkHalideBuf<double>(numOfVis,5)
    , gcf_buffer = mkHalideBuf<double>(over2, gcfSize, gcfSize, 2)
    , uvg_buffer = mkHalideBuf<double>(gridSize, gridSize, 2)
    ;
  vis_buffer.host = tohost(vis.data());
  gcf_buffer.host = tohost(gcf.data());
  uvg_buffer.host = tohost(uvg.data());

  printf("Start gridding!\n");

  clock_t ti;
  
#define _RUNME(k)                                                     \
  memset(uvg.data(), 0, fullSize * 2 * sizeof(double));               \
  printf("%s duration: ", #k);                                        \
  ti = clock();                                                       \
  if ( k(t2, gridSize, &vis_buffer, &gcf_buffer, &uvg_buffer) != 0 ){ \
    printf("Broken!");                                                \
    return -3;                                                        \
  }                                                                   \
  printf("%ld\n", clock() - ti);

  // _RUNME(kern_scatter_bare)
  _RUNME(kern_scatter_bare0123)
  _RUNME(kern_scatter_bare0132)
  // _RUNME(kern_scatter_bare0213)
  // VERY SLOW! _RUNME(kern_scatter_bare0231)
  // _RUNME(kern_scatter_bare0312)
  // VERY SLOW! _RUNME(kern_scatter_bare0321)
  // VERY SLOW! _RUNME(kern_scatter_bare1230)
  // _RUNME(kern_scatter_bare2)
  // _RUNME(kern_scatter_bare12)
  // _RUNME(kern_scatter_dims)
  _RUNME(kern_scatter_dims0123)
  _RUNME(kern_scatter_dims0132)
  // _RUNME(kern_scatter_dims12)

  normalizeCPU(
      reinterpret_cast<complexd*>(uvg.data())
    , gridPitch
    , gridSize
    );

  printf("Write!\n");
  writeImgToDisk("grid.dat", reinterpret_cast<complexd*>(uvg.data()));

  return 0;
}
