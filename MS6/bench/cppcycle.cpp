#include <vector>
#include <complex>
#include <fstream>
#include <ctime>
#include <thread>

#include "kernels_halide_gpu.h"
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
const int gcf_pad = 0;
const int over2 = over*over;
const int gcf_storage_size = over * gcf_size * (over * gcf_size + gcf_pad);
// const int grid_size = 8192;
const int grid_size = 2048;

const int grid_pitch = grid_size + grid_pad;
const int full_size = grid_pitch * grid_size;

const int num_of_vis = num_baselines * num_times;
const int num_of_doubles = num_of_vis * 5;

template <typename T>
void writeImgToDisk(const char * fname, T * out){
  // FILE * f = fopen(fname, "wb");
  FILE * f;
  // if (f == NULL) {
  if(fopen_s(&f, fname, "wb") != 0){
    printf("Can't open %s\n", fname);
    return;
  }
  printf("Writing %s ...\n", fname);
  for (int r = 0; r < grid_size; r++, out += grid_pitch)
    fwrite(out, sizeof(T), grid_size, f);
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
  vecd vis(num_of_doubles);
  printf("Read visibilities!\n");
  res = readFileToVector(vis, "vis.dat"); __CK
  vecd gcf(gcf_storage_size * 2); // complex
  printf("Read GCF!\n");
  res = readFileToVector(gcf, GCF_FILE); __CK

  vecd uvg(full_size * 2, 0); // complex

  buffer_t
      vis_buffer = mkHalideBuf<double>(num_of_vis,5)
    , gcf_buffer = mkHalideBuf<double>(over2, gcf_size, gcf_size, 2)
    , uvg_buffer = mkHalideBuf<double>(grid_size, grid_size, 2)
    ;

  vis_buffer.host = tohost(vis.data());
  gcf_buffer.host = tohost(gcf.data());
  uvg_buffer.host = tohost(uvg.data());

  // Juct to make args different
  auto tfunc = [&](int n, double s) {
    return [&] {
      volatile clock_t ti = clock();
      printf("%d started!\n", n); 
      if ( kern_scatter_gpu(t2/s, grid_size, &vis_buffer, &gcf_buffer, &uvg_buffer) != 0 ){
        printf("Broken!\n"); 
      }
      printf("%d finished in %f secs!\n", n, (double)(clock() - ti) / CLOCKS_PER_SEC); 
    };
  };

  std::thread th1(tfunc(1, 1.0));
  std::thread th2(tfunc(2, 1.5));
  th1.join();
  th2.join();
  printf("Done. Normalizing ...\n");
  
  normalizeCPU(
      reinterpret_cast<complexd*>(uvg.data())
    , grid_pitch
    , grid_size
    );

  printf("Write!\n");
  writeImgToDisk("grid.dat", reinterpret_cast<complexd*>(uvg.data()));

  return 0;
}
