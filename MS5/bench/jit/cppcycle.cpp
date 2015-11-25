#include <vector>
#include <complex>
#include <fstream>
#include <ctime>
#include <cstring>
#include <omp.h>

#include "scatter_halide.h"
#include "../mkHalideBuf.h"
#include "cfg.h"
#include "Halide.h"

using namespace std;
using namespace Halide;

typedef complex<double> complexd;

// Config
const double t2 = 0.2/2.0;
const int over = OVER;
const int pad = 0;
const int over2 = over*over;
const int gridSize = 8192;
const int gridPad = 0;

const int gridPitch = gridSize + gridPad;
const int fullSize = gridPitch * gridSize;

const int numOfVis = num_baselines * num_times;
const int numOfDoubles = numOfVis * 5;

typedef int (*halide_task)(void *user_context, int, uint8_t *);

struct JITHandlers {
    void (*custom_print)(void *, const char *);
    void *(*custom_malloc)(void *, size_t);
    void (*custom_free)(void *, void *);
    int (*custom_do_task)(void *, halide_task, int, uint8_t *);
    int (*custom_do_par_for)(void *, halide_task, int, int, uint8_t *);
    void (*custom_error)(void *, const char *);
    int32_t (*custom_trace)(void *, const halide_trace_event *);
    JITHandlers() : custom_print(NULL), custom_malloc(NULL), custom_free(NULL),
                    custom_do_task(NULL), custom_do_par_for(NULL),
                    custom_error(NULL), custom_trace(NULL) {
    }
};

struct JITUserContext {
    void *user_context;
    JITHandlers handlers;
};

typedef int32_t (*kernfun_t)(buffer_t *, buffer_t *, int32_t, double, JITUserContext *, buffer_t *);

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

int64_t clock_diff(struct timespec *ts) {
    timespec ts2;
    clock_gettime(CLOCK_REALTIME, &ts2);
    return (int64_t)(ts2.tv_sec - ts->tv_sec) * 1000000000 +
           (int64_t)(ts2.tv_nsec - ts->tv_nsec);
}

void runGridder(int nthreads, SGridderConfig cfg, const vecd &vis,
                const vecd &gcf, const vecd &uvg)
{
  SGridder gridder(cfg);

  // Use this to figure out the argument order for the kernel. The
  // general scheme is:
  //  1. All input buffers in alphabetical order
  //  2. All scalar parameters in alphabetical order
  //  3. The JIT user context
  //  4. The output buffer(s) in alphabetical order
  // The call below will give 1+2, but not 3+4!
  //for( Argument a : gridder.uvg.infer_arguments() ) { printf("%s ", a.name.data()); }
  //puts("");

  // Compile. We measure the time, for sanity-checks
  Target target_plain(get_target_from_environment().os, Target::X86, 64, { Target::SSE41, Target::AVX});
  target_plain.set_feature(Target::NoAsserts, true);
  timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  kernfun_t kernfun = reinterpret_cast<kernfun_t>(gridder.uvg.compile_jit(target_plain));
  printf("\t%ld\t -", clock_diff(&ts)); fflush(stdout);

  // Calculate number of chunks
  int nchunks = numOfVis / cfg.steps / cfg.chunks;

  for (int i = 0; i < 4; i++) {
    clock_gettime(CLOCK_REALTIME, &ts);

#pragma omp parallel for num_threads(nthreads)
    for (int j = 0; j < nthreads; j++) {
        memset(tohost(uvg.data() + j * fullSize * 2), 0, fullSize * 2 * sizeof(double));
    }

#pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    for (int chunk = nchunks-1; chunk >= 0; chunk--){

      // Set input and output buffers for Halide. It should only use
      // an appropriate chunk of visibilities and output to its own grid.
      int vis0 = cfg.steps * cfg.chunks * chunk;
      int vis1 = cfg.steps * cfg.chunks * (chunk + 1);
      if (chunk == nchunks - 1) vis1 = numOfVis;

      buffer_t
          vis_buffer = mkHalideBuf<double>(vis1 - vis0,5)
        , gcf_buffer = mkHalideBuf<double>(over2, cfg.gcfSize, cfg.gcfSize, 2)
        , uvg_buffer = mkHalideBuf<double>(cfg.gridSize, cfg.gridSize, 2)
        ;
      vis_buffer.host = tohost(vis.data() + 5 * vis0);
      vis_buffer.min[1] = vis0;
      gcf_buffer.host = tohost(gcf.data());
      uvg_buffer.host = tohost(uvg.data() + omp_get_thread_num() * fullSize * 2);

      JITUserContext jit_context; timespec ts2;
      clock_gettime(CLOCK_REALTIME, &ts2);
      kernfun(&gcf_buffer, &vis_buffer, cfg.gridSize, t2, &jit_context, &uvg_buffer);
    }

    printf("\t%ld", clock_diff(&ts)); fflush(stdout);

  }

}

int main(/* int argc, char * argv[] */)
{
  // Get number of threads from environment
  int nthreads;
  if (getenv("OMP_NUM_THREADS")) {
      sscanf(getenv("OMP_NUM_THREADS"), "%d", &nthreads);
  } else {
      nthreads = 4;
  }
  printf("Thread count: %d\n", nthreads);

  // Data set-up
  int res;
  vecd vis(numOfDoubles);
  printf("Read visibilities!\n"); fflush(stdout);
  res = readFileToVector(vis, "vis.dat"); __CK;

  // Create a uv-grid for every thread
  vecd uvg(fullSize * 2 * nthreads); // complex, per thread

  // Loop through configurations
  for (int gcfSize = 16; gcfSize <= 64; gcfSize *= 2) {
    const int gcfStorageSize = over * gcfSize * (over * gcfSize + pad);
    vecd gcf(gcfStorageSize * 2); // complex
    printf("Read GCF!\n"); fflush(stdout);

    char name[32];
    sprintf(name, "gcf%d.dat", gcfSize);
    res = readFileToVector(gcf, name); __CK;
    for (int dim = 0; dim < (1 << _DIM_CONFIGS); dim++) {

    // These are generally beneficial
    if (!(dim & (1 << _UVG0))) continue;
    if (!(dim & (1 << _UVG1))) continue;
    if (!(dim & (1 << _GCF0))) continue;
    if (!(dim & (1 << _GCF1))) continue;
    for (int upd = 0; upd <= _UPD_UNROLL; upd++) {

    // Update strategy 2 never seems to win out
    if (upd == _UPD_FUSE) continue;

    for (int vector = 2; vector <= 8; vector*=2) {
    if (upd < _UPD_FUSE && vector > 2) break;
    for (int pos = 0; pos < 3; pos++) {
    for (int chunks = 20; chunks < 40; chunks += 5) {

      // Create a gridder configuration
      SGridderConfig cfg;
      cfg.gcfSize = gcfSize;
      cfg.gridSize = gridSize;
      switch(pos) {
      case 0: cfg.cpos = 0; cfg.xpos = 1; cfg.ypos = 2; cfg.vpos = 3; cfg.blpos = 4; break;
      case 1: cfg.cpos = 0; cfg.xpos = 1; cfg.ypos = 4; cfg.vpos = 2; cfg.blpos = 3; break;
      case 2: cfg.cpos = 0; cfg.xpos = 1; cfg.ypos = 3; cfg.vpos = 2; cfg.blpos = 4; break;
      }
      cfg.dim = dim;
      cfg.upd = (UpdConfig) upd;
      cfg.vector = vector;
      cfg.steps = 200;
      cfg.chunks = chunks;

      // Run
      printf("pos %d dim %d upd %d vector %d steps %d chunks %d -", pos, cfg.dim, cfg.upd, cfg.vector, cfg.steps, cfg.chunks); fflush(stdout);
      runGridder(nthreads, cfg, vis, gcf, uvg);
      puts("");

    }}}}}

  }
  return 0;
}
