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
  const int nb = gsiz / siz;
  
  void put_point(const pre & p, const visData & vis){
    bins[p.u / siz][p.v / siz].push_back(vis);
  }

  vector<visData> bins[gsiz / siz][gsiz / siz];
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
  int n_in = 0;
  for(const visData & vd : vis) {
    pre p;
    p = prep(t2, vd);
    if (inbound(p)){
      n_in++;
      ss.put_point(p, vd);
    }
  }
  printf("%d of points are in bounds - %5.3f of the total number\n", n_in, double(n_in)/double(num_of_vis));

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
    , uvg_buffer = mkHalideBuf<double>(grid_size, grid_size, 2)
    ;

  gcf_buffer.host = tohost(gcf.data());
  uvg_buffer.host = tohost(uvg.data());

  const halide_device_interface * cuface = halide_cuda_device_interface();

  printf("Marshal zero grid and GCF to GPU!\n");

  // We manually marshal the data to GPU
  // and set 'host_dirty' to false and host pointer to null
  // to *prevent* each kernel from creating it's own copy
  // of uvg and gcf buffers on GPU and marshaling the data into them
  // FIXME: now each kernel implicitly marshals it's part of visibility data
  // to GPU and this time is added to a kernel time.
  // Make visibility data marshalling a separate step!
  res = halide_copy_to_device(nullptr, &gcf_buffer, cuface); __CK
  gcf_buffer.host_dirty = false; gcf_buffer.host = nullptr;

  res = halide_copy_to_device(nullptr, &uvg_buffer, cuface); __CK
  uvg_buffer.host_dirty = false; uvg_buffer.host = nullptr;

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
      buffer_t vis_buffer = mkHalideBuf<double>(ss.bins[i][j].size(),5);
      vis_buffer.host = tohost(vis.data());
      threads.push_back(thread(mk_grid_func(vis_buffer)));
    }

    for (thread & t : threads) t.join();
  };

  run_bunch(0,0);
  run_bunch(0,1);
  run_bunch(1,0);
  run_bunch(1,1);

  printf("Finished gridding!\nMarshal the result back to CPU.\n");
    
  uvg_buffer.host = tohost(uvg.data());
  res = halide_copy_to_host(nullptr, &uvg_buffer); __CK
  printf("Done.\n");

  return 0;
}
