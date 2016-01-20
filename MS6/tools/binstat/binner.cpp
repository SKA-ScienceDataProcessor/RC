#include <cstring>
#include <iomanip>
#include <fstream>

#include <algorithm>
#include <array>
#include <vector>
#include <complex>

using namespace std;

typedef complex<double> complexd;

const int over = 8;
const int gcf_size = 16;
const int grid_size = 8192;
const double t2 = 0.2/2.0;

const int
    num_baselines = 32131
  , num_times = 200
  , numOfVis = num_baselines * num_times
  ;

struct visData {
  double u, v, w;
  complexd amp;
};

struct pre {
  short u, v, overu, overv;
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
    , overu = short(floor(over * (us - u)))
    , overv = short(floor(over * (vs - v)))
    ;
  u += short(grid_size / 2 /* - gcf_size / 2 */);
  v += short(grid_size / 2 /* - gcf_size / 2 */);
  return {u, v, overu, overv};
}

bool inbound(const pre & p){
  return (  p.u >= 0 && p.u < grid_size
         && p.v >= 0 && p.v < grid_size
         );
}

template<int gsiz, int siz>
struct thread_map : vector<unsigned int>{
  thread_map() : vector((gsiz/siz)*(gsiz/siz)) { for(unsigned int &i : *this) i = 0U; }
  void put_point(const pre & p) {
    unsigned int latx, laty;
    latx = p.u / siz;
    laty = p.v / siz;
    (*this)[laty * (gsiz/siz) + latx]++;
  }

  unsigned int at(int y, int x){return (*this)[y * (gsiz/siz) + x];}

  void dump(ofstream & of){
    for(int y=0; y<(gsiz/siz); y++) {
      for(int x=0; x<(gsiz/siz); x++) {
        char out[256];
        unsigned int n;
        n = at(y,x);
        if (n > 0) {
          sprintf_s(out, 256, "at (%4d, %4d): %4d", y, x, n);
          of << out << endl;
        }
      }
    }
  }
};

template <int gsiz, int siz>
struct streamset {
  void put_point(const pre & p, const complexd & vis){
    unsigned int binx, biny;
    binx = p.u % siz;
    biny = p.v % siz;
    pre_streams[biny][binx].push_back(p);
    vis_streams[biny][binx].push_back(vis);
    thread_maps[biny][binx].put_point(p);
  }

  // We guess that data are almost evenly
  //   distributed.
  // We put 1.25 factor for "almost"
  void reserve(int n){
    int rn = int( 1.25 * double(n) / double(siz*siz) );
    for_each(&pre_streams[0][0], &pre_streams[siz][0], [rn](auto p){p.reserve(rn);});
    for_each(&vis_streams[0][0], &vis_streams[siz][0], [rn](auto v){v.reserve(rn);});
  }

  vector<pre>      pre_streams[siz][siz];
  vector<complexd> vis_streams[siz][siz];
  thread_map<gsiz, siz> thread_maps[siz][siz];
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
  vector<visData> vis(numOfVis);

  printf("Read visibilities!\n");

  // MS5 (quintuples) format
  #ifdef _WIN32 // My desktop
  #define __VIS_PATH "G:\\BR\\MS5\\bench\\vis.dat"
  #else
  #define __VIS_PATH "vis.dat"
  #endif
  res = readFileToVector(vis, __VIS_PATH); __CK

  streamset<grid_size, gcf_size> ss;
  // IIRC, we had 0.815 of all points "in bound" with our data.
  ss.reserve(int(0.815 * numOfVis));

  int n_in = 0;
  for(const visData & vd : vis) {
    pre p;
    p = prep(t2, vd);
    if (inbound(p)){
      n_in++;
      ss.put_point(p, vd.amp);
    }
  }
  printf("%d of points are in bounds - %5.3f of the total number\n", n_in, double(n_in)/double(numOfVis));

  ofstream logf("log.txt");

  for (int i=0; i < gcf_size; i++)
  for (int j=0; j < gcf_size; j++){
    char fname[260];
    sprintf_s(fname, 260, "%02d_%02d.txt", i, j);
    { ofstream of(fname);
      short oldu = 0, oldv = 0;
      for(auto p : ss.pre_streams[i][j]) {
        if (p.u != oldu || p.v != oldv) {
          of << "-------------\n";
          oldu = p.u;
          oldv = p.v;
        }
        of << p.u << " " << p.v << endl;
      }
    }
    sprintf_s(fname, 260, "%02d_%02d_tm.txt", i, j);
    { ofstream of(fname);
      ss.thread_maps[i][j].dump(of);
    }

    char out[256];
    sprintf_s(out, 256, "(%2d, %2d) : %lld\n", i, j, ss.pre_streams[i][j].size());
    logf << out;
  }

  return 0;
}
