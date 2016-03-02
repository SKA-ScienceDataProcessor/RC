#define _CRT_SECURE_NO_WARNINGS

#include <vector>
#include <complex>
#include <fstream>
// #include <ctime>

#ifdef _MSC_VER
#pragma warning(push, 0)
#endif
#include "Halide.h"
#ifdef _MSC_VER
#pragma warning(pop)
#endif
#include "kernels_halide_gpu.h"
#include "mkHalideBuf.h"
#include "cfg.h"

using namespace std;

typedef complex<double> complexd;

// Config
const double t2 = 0.2/2.0;
const int over = OVER;
const int pad = 0;
const int over2 = over*over;
const int gcfSize = GCF_SIZE;
const int gcfStorageSize = over * gcfSize * (over * gcfSize + pad);
// const int gridSize = 8192;
const int gridSize = 2048;
const int gridPad = 0;

const int gridPitch = gridSize + gridPad;
const int fullSize = gridPitch * gridSize;

const int numOfVis = num_baselines * num_times;
const int numOfDoubles = numOfVis * 5;

#ifndef _WIN32
#define TTF "%ld"
static timespec ts;
void clock_start(){
  clock_gettime(CLOCK_REALTIME, &ts);
}
time_t clock_diff() {
  timespec ts2;
  clock_gettime(CLOCK_REALTIME, &ts2);
  return (time_t)(ts2.tv_sec - ts.tv_sec) * 1000000000 +
         (time_t)(ts2.tv_nsec - ts.tv_nsec);
}
#else
#define TTF "%lld"
static time_t t;
void clock_start(){t = clock();}
time_t clock_diff() {return clock() - t;}
#endif

template <typename T>
void writeImgToDisk(const char * fname, T * out){
  FILE * f = fopen(fname, "wb");
  // FILE * f;
  if (f == NULL) {
  // if(fopen_s(&f, fname, "wb") != 0){
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

#define __CK if (res != 0) {printf("Error: %d\n", res); return res; }

int main(/* int argc, char * argv[] */)
{
  int res;
  vecd vis(numOfDoubles);

  printf("Read visibilities!\n");
  // MS5 (quintuples) format
  #ifdef _WIN32 // My desktop
  #define __VIS_PATH "G:\\BR\\MS5\\bench\\vis.dat"
  #else
  #define __VIS_PATH "vis.dat"
  #endif
  res = readFileToVector(vis, __VIS_PATH); __CK

  vecd gcf(gcfStorageSize * 2); // complex

  printf("Read GCF!\n");
  #define __STR(a) #a
  #ifdef _WIN32 // My desktop
  #define __GCF_PATH(sz) "G:\\BR\\MS5\\bench\\gcf" __STR(sz) ".dat"
  #else
  #define __GCF_PATH(sz) "gcf" __STR(sz) ".dat"
  #endif
  res = readFileToVector(gcf, __GCF_PATH(GCF_SIZE)); __CK

  vecd uvg(fullSize * 2, 0.0); // complex

  buffer_t
      vis_buffer = mkHalideBuf<double>(numOfVis,5)
    , gcf_buffer = mkHalideBuf<double>(over2, gcfSize, gcfSize, 2)
    , uvg_buffer = mkHalideBuf<double>(gridSize, gridSize, 2)
    ;

  vis_buffer.host = tohost(vis.data());
  gcf_buffer.host = tohost(gcf.data());
  uvg_buffer.host = tohost(uvg.data());

  vis_buffer.host_dirty = true;
  gcf_buffer.host_dirty = true;
  uvg_buffer.host_dirty = true;

#if 0
  // Spare run ... only to copy vis and gcf to the device ...
  kern_scatter_gpu(t2, gridSize, &vis_buffer, &gcf_buffer, &uvg_buffer);
  halide_device_sync(nullptr, &uvg_buffer);
  // Ensure dirty bits are cleared ...
  vis_buffer.host_dirty = false;
  gcf_buffer.host_dirty = false;
  uvg_buffer.dev_dirty = false;
#endif

  printf("GPU gridder started!\n"); 
  clock_start();
  if ( kern_scatter_gpu(t2, gridSize, &vis_buffer, &gcf_buffer, &uvg_buffer) != 0 ){
    printf("Broken!\n"); 
  }
  // halide_device_sync(nullptr, &uvg_buffer);
  printf("GPU gridder finished in " TTF "!\nNormalizing ...\n", clock_diff()); fflush(stdout);
  // halide_copy_to_host(nullptr, &uvg_buffer);

  clock_start();
  normalizeCPU(
      reinterpret_cast<complexd*>(uvg.data())
    , gridPitch
    , gridSize
    );
  printf("Normalized in " TTF ".\nWrite!\n", clock_diff()); fflush(stdout);
  writeImgToDisk("grid.dat", reinterpret_cast<complexd*>(uvg.data()));

  return 0;
}
