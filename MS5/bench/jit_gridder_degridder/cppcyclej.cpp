#include <vector>
#include <complex>
#include <fstream>
#include <ctime>
#include <cstring>

#ifdef __USE_GPU
#include <cuda.h>
#define __SYNC cuCtxSynchronize();
#else
#define __SYNC
#endif

#include "degrid_halide.h"
#include "scatter_halide.h"

#include "utils.h"
#include "mkHalideBuf.h"
#include "cfg.h"

#ifdef GCF32
#define GCF_FILE "gcf32.dat"
#else
#define GCF_FILE "gcf16.dat"
#endif

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
  printf("Read visibilities!\n"); fflush(stdout);
  res = readFileToVector(vis, "vis.dat"); __CK
  vecd gcf(gcfStorageSize * 2); // complex
  printf("Read GCF!\n"); fflush(stdout);
  res = readFileToVector(gcf, GCF_FILE); __CK

  vecd uvg(fullSize * 2); // complex

  buffer_t
      vis_buffer = mkHalideBuf<double>(numOfVis, _VIS_FIELDS)
    , gcf_buffer = mkHalideBuf<double>(over2, gcfSize, gcfSize, _CPLX_FIELDS)
    , uvg_buffer = mkHalideBuf<double>(gridSize, gridSize, _CPLX_FIELDS)
    ;
  vis_buffer.host = tohost(vis.data());
  gcf_buffer.host = tohost(gcf.data());
  uvg_buffer.host = tohost(uvg.data());

  // Aliased buffers for degridder
  buffer_t
      uvw_buffer = vis_buffer
    , vis_out_buffer = vis_buffer
    ;
  // fix them
  uvw_buffer.extent[0] = _UVWFields;
  vis_out_buffer.host += _UVWFields * sizeof(double);
  vis_out_buffer.extent[0] = _VIS_FIELDS - _UVWFields;

  Buffer
      visbuf(type_of<double>(), &vis_buffer, "vis")
    , gcfbuf(type_of<double>(), &gcf_buffer, "gcf")
    , uvgbuf(type_of<double>(), &uvg_buffer, "uvg")
    , uvwbuf(type_of<double>(), &uvw_buffer, "uvw")
    , visoutbuf(type_of<double>(), &vis_out_buffer, "visout")
    ;


  Target target(get_jit_target_from_environment()
#ifdef __USE_GPU
                  .with_feature(Target::CUDA)
#ifdef __ON_WILKES
                  .with_feature(Target::CUDACapability35)
#endif
#endif
                  );

  for (int pos = 0; pos < 2; pos++) {
    printf("pos %d:\t", pos);

    int cpos, xpos, ypos, vpos;
    switch(pos) {
      case 0: cpos = 0; xpos = 1; ypos = 2; vpos = 3; break;
      case 1: cpos = 0; xpos = 1; ypos = 3; vpos = 2; break;
    }
    SGridder gridder(cpos, xpos, ypos, vpos);

    gridder.vis.set(visbuf);
    gridder.gcf_fused.set(gcfbuf);
    gridder.scale.set(t2);

    // Could move out of the loop, but leave it here for a while
    SDeGridder degridder(0, 1, 2);

    degridder.uvw.set(uvwbuf);
    degridder.uvg.set(uvgbuf);
    degridder.gcf_fused.set(gcfbuf);
    degridder.scale.set(t2);

    printf("Compiling... ");
    gridder.uvg.compile_jit(target);
    degridder.vis.compile_jit(target);
    printf("Done.\n");

#ifdef __USE_GPU
    // Marshalling all data to device
    visbuf.copy_to_device();
    gcfbuf.copy_to_device();
#endif
    for (int i = 0; i < /*4*/1; i++) {
      // Quick hack here (to not cope with Buffer guts):
      // instead of memsetting device memory we marshal zeroed uvg array to the device
      memset(uvg.data(), 0, fullSize * 2 * sizeof(double));
#ifdef __USE_GPU
      uvgbuf.copy_to_device();
#endif

      clock_start();
      gridder.uvg.realize(uvgbuf);
      __SYNC
      printf(TTF "/", clock_diff()); fflush(stdout);

      clock_start();
      degridder.vis.realize(visoutbuf);
      __SYNC
      printf(TTF "\t", clock_diff()); fflush(stdout);
    }
    puts("");

  }

  return 0;
}
