#include <vector>
#include <complex>
#include <fstream>
#include <cstring>

#include "cfg.h"

#ifdef GCF32
#define GCF_FILE "gcf32.dat"
#else
#define GCF_FILE "gcf16.dat"
#endif

#include "scatter_gridder_fixed_gcf_5t_vis.h"

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

  printf("Start gridding!\n");
  memset(uvg.data(), 0, fullSize * 2 * sizeof(double));
  printf("%s duration: ", "Pure CPU");

  typedef const complexd (*gcf_t)[OVER][OVER][GCF_SIZE][GCF_SIZE];
  typedef const visData (*data_t)[num_baselines][num_times];
  gridKernel_scatter_full(
      t2
    , reinterpret_cast<complexd*>(uvg.data())
    , *reinterpret_cast<gcf_t>(gcf.data())
    , *reinterpret_cast<data_t>(vis.data())
    , gridSize
    );

  normalizeCPU(
      reinterpret_cast<complexd*>(uvg.data())
    , gridPitch
    , gridSize
    );

  printf("Write!\n");
  writeImgToDisk("grid.dat", uvg.data());

  return 0;
}
