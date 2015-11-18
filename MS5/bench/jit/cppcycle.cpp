#include <vector>
#include <complex>
#include <fstream>
#include <ctime>
#include <cstring>

#include "scatter_halide.h"
#include "../mkHalideBuf.h"
#include "cfg.h"
#include "Halide.h"

#ifdef GCF32
#define GCF_FILE "gcf32.dat"
#else
#define GCF_FILE "gcf16.dat"
#endif

using namespace std;
using namespace Halide;

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

typedef int (*kern_t)(const double _scale, const int32_t _grid_size, buffer_t *_vis_buffer, buffer_t *_gcf_buffer, buffer_t *_uvg__2_buffer);
    
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

  Target target_plain(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});

  for (int pos = 0; pos < 2; pos++) {
  for (int dim = 0; dim < (1 << _DIM_CONFIGS); dim++) {
  for (int upd = 0; upd <= _UPD_FUSE_UNROLL; upd++) {
  for (int vector = 2; vector <= 32; vector*=2) {
  if (upd < _UPD_FUSE && vector > 2) break;

  SGridderConfig cfg;
  switch(pos) {
  case 0: cfg.cpos = 0; cfg.xpos = 1; cfg.ypos = 2; cfg.vpos = 3; break;
  case 1: cfg.cpos = 0; cfg.xpos = 1; cfg.ypos = 3; cfg.vpos = 2; break;
  }
  cfg.dim = dim;
  cfg.upd = (UpdConfig) upd;
  cfg.vector = vector;
  SGridder gridder  = SGridder(cfg);

  gridder.vis.set(Buffer(Float(64), &vis_buffer, "vis"));
  gridder.gcf_fused.set(Buffer(Float(64), &gcf_buffer, "gcf"));
  gridder.scale.set(t2);
  gridder.grid_size.set(gridSize);

  printf("pos %d dim %d upd %d vector %d -", pos, dim, upd, vector);

  clock_t ti = clock();
  gridder.uvg.compile_jit(target_plain);
  printf("\t%ld\t -", clock() - ti);

  for (int i = 0; i < 2; i++) {
      memset(uvg.data(), 0, fullSize * 2 * sizeof(double));
      ti = clock();
      gridder.uvg.realize(Buffer(Float(64), &uvg_buffer, "uvg"));
      printf("\t%ld", clock() - ti);
  }
  puts("");
  }}}}

  return 0;
}
