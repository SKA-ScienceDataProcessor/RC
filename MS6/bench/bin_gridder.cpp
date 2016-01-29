#include <cstring>
#include <iomanip>
#include <fstream>

#include <algorithm>
#include <array>
#include <vector>
#include <complex>

#include <thread>

#include "Halide.h"
#include "HalideRuntimeCuda.h"

#include "kernels_halide_gpu.h"

#include "mkHalideBuf.h"
#include "cfg.h"

using namespace std;

typedef complex<double> complexd;

const int over2 = over*over;
const int gcf_storage_size = over * gcf_size * (over * gcf_size + gcf_pad);
const int grid_pitch = grid_size + grid_pad;
const int full_size = grid_pitch * grid_size;
const int num_of_vis = num_baselines * num_times;

struct visData {
  double u, v, w;
  complexd amp;
};

struct pre {
  short u, v;
};

inline
pre prep(double scale, const visData & vd){
  double
      us = vd.u * scale
    , vs = vd.v * scale
    ;
  short
      u = short(floor(us))
    , v = short(floor(vs))
    ;
  u += short(grid_size / 2 /* - gcf_size / 2 */);
  v += short(grid_size / 2 /* - gcf_size / 2 */);
  return {u, v};
}

bool inbound(const pre & p){
  return (  p.u >= 0 && p.u < grid_size
         && p.v >= 0 && p.v < grid_size
         );
}

template <int gsiz, int siz>
struct streamset {
  streamset() : n_in(0) {};

  const int nb = gsiz / siz;
  
  void put_point(const pre & p, const visData & vis){
    bins[p.u / siz][p.v / siz].push_back(vis);
    n_in++;
  }

  int n_in;
  vector<visData> bins[gsiz / siz][gsiz / siz];
  buffer_t bufs[gsiz / siz][gsiz / siz];
};

// v should be preallocated with right size
template <typename T>
int readFileToVector(vector<T> & v, const char * fname){
  ifstream is(fname, ios::binary);
  if (is.fail()) {
    printf("Can't open %s.\n", fname);
    return -1;
  }
  is.read(reinterpret_cast<char*>(v.data()), v.size() * sizeof(T));
  if (is.fail()) {
    printf("Can't read %s.\n", fname);
    return -2;
  }
  return 0;
}

#define __CK if (res < 0) { printf("Err: %d\n", res); return res; }

int main(/* int argc, char * argv[] */)
{
  int res;
  vector<visData> vis(num_of_vis);

  printf("Read visibilities!\n");

  // MS5 (quintuples) format
  #ifdef _WIN32 // My desktop
  #define __VIS_PATH "G:\\BR\\MS5\\bench\\vis.dat"
  #else
  #define __VIS_PATH "vis.dat"
  #endif
  res = readFileToVector(vis, __VIS_PATH); __CK

  streamset<grid_size, gcf_size> ss;

  printf("Start binning!\n");
  for(const visData & vd : vis) {
    pre p;
    p = prep(t2, vd);
    if (inbound(p)){
      ss.put_point(p, vd);
    }
  }
  printf("%d of points are in bounds - %5.3f of the total number\n", ss.n_in, double(ss.n_in)/double(num_of_vis));

  vector<complexd> gcf(gcf_storage_size);

  printf("Read GCF!\n");

  #define __STR(a) #a
  #ifdef _WIN32 // My desktop
  #define __GCF_PATH(sz) "G:\\BR\\MS5\\bench\\gcf" __STR(sz) ".dat"
  #else
  #define __GCF_PATH(sz) "gcf" __STR(sz) ".dat"
  #endif
  res = readFileToVector(gcf, __GCF_PATH(GCF_SIZE)); __CK

  // FIXME: We don't bother allocating vector on GPU and zero it,
  //   we simply marshal zero vector from CPU to GPU.
  vector<complexd> uvg(full_size, {0.0, 0.0});

  buffer_t
      gcf_buffer = mkHalideBuf<double>(over2, gcf_size, gcf_size, 2)
    , uvg_buffer = mkHalideBuf<double>(grid_pitch, grid_size, 2)
    ;

  gcf_buffer.host = tohost(gcf.data());
  uvg_buffer.host = tohost(uvg.data());

  const halide_device_interface * cuface = halide_cuda_device_interface();

  printf("Marshal zero grid and GCF to GPU!\n");

  #define __RESET(b)       \
     b.host_dirty = false; \
     b.dev_dirty = false;  \
     b.host = nullptr;

  res = halide_copy_to_device(nullptr, &gcf_buffer, cuface); __CK
  __RESET(gcf_buffer)
  res = halide_copy_to_device(nullptr, &uvg_buffer, cuface); __CK
  __RESET(uvg_buffer)
  
  // FIXME: I tried to make the single `sizeof(visData) * ss.n_in` bytes
  // allocation on device and then only marshal the data to already
  // allocated memory, but that didn't worked out-of-the-box.
  // Investigate further!
  for (int i=0; i < ss.nb; i++)
  for (int j=0; j < ss.nb; j++)
  if (! ss.bins[i][j].empty()) {
    ss.bufs[i][j] = mkHalideBuf<double>(ss.bins[i][j].size(), sizeof(visData)/sizeof(double));
    ss.bufs[i][j].host = tohost(ss.bins[i][j].data());
    res = halide_copy_to_device(nullptr, &ss.bufs[i][j], cuface); __CK
    __RESET(ss.bufs[i][j])
  }

  auto mk_grid_func = [&](buffer_t vis_buffer) {
    return [&] {
      kern_scatter_gpu(t2, grid_size, &vis_buffer, &gcf_buffer, &uvg_buffer);
    };
  };

  printf("Start multithreaded gridding on bins!\n");

  auto run_bunch = [&](int si, int sj){
    vector<thread> threads;

    for (int i=si; i < ss.nb; i+=2)
    for (int j=sj; j < ss.nb; j+=2)
    if (! ss.bins[i][j].empty()) {
      threads.push_back(thread(mk_grid_func(ss.bufs[i][j])));
    }

    for (thread & t : threads) t.join();
    printf("Bunch (%d,%d) finished!\n", si, sj);
  };

  run_bunch(0,0);
  run_bunch(0,1);
  run_bunch(1,0);
  run_bunch(1,1);

  printf("Finished gridding!\nMarshal the result back to CPU.\n");
    
  uvg_buffer.host = tohost(uvg.data());
  res = halide_copy_to_host(nullptr, &uvg_buffer); __CK
  printf("Done.\n");

  // FIXME: Add cleanup (for purists).
  return 0;
}
