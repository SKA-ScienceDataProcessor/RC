/* 
  Copyright (C) 2015 Braam Research, LLC.
 */
#include <cstdio>
#include <cstring>
#include <utility>

using namespace std;

#define WPLANES 32
#define TIMESTEPS 200
#define BASELINES 32131

// Very quick and dirty.
// We already know we have no more than
//   2 slices per baseline.
pair<int,int> slices[BASELINES][2];

struct baseline_descr {
  int bl;
  int items;
};

void proc(int wplane) {
  static char name[64];
  sprintf(name, "wplanes/%02d_wplane.dat", wplane);

  FILE * f = fopen(name, "rb");
  if (f == nullptr) return;

  printf("At %02d wplane:\n", wplane);
  fseek(f, -sizeof(int), SEEK_END);
  int num_baselines;
  fread(&num_baselines, sizeof(int), 1, f);
  printf("File %s contains info for %d baselines:\n", name, num_baselines);
  fseek(f, -(sizeof(int) + sizeof(baseline_descr) * num_baselines), SEEK_END);

  for(int i=0; i < num_baselines; i++) {
    baseline_descr bld;
    fread(&bld, sizeof(baseline_descr), 1, f);
    const char * tmark;
    if (bld.items != TIMESTEPS) tmark = " TRANS";
    else tmark = "";
    printf("%8d %8d%s\n", bld.bl, bld.items, tmark);
    if (slices[bld.bl][0].second == 0) {
      slices[bld.bl][0].first = wplane;
      slices[bld.bl][0].second = bld.items;
    }
    else {
      slices[bld.bl][1].first = wplane;
      slices[bld.bl][1].second = bld.items;
    }
  }
}

int main(){
  for(int i=0; i < BASELINES; i++) {
    slices[i][0] = {0, 0};
    slices[i][1] = {0, 0};
  }
  for(int i=0; i < WPLANES; i++) proc(i);
  int sliced = 0;
  for(int i=0; i < BASELINES; i++) {
    if (slices[i][1].second != 0) {
      sliced++;
      printf("Baseline %2d is sliced between 2 planes thus: %2d[%2d] %2d[%2d]\n", i
        , slices[i][0].first, slices[i][0].second
        , slices[i][1].first, slices[i][1].second
        );
    }
  }
  printf("%d sliced baselines total.\n", sliced);
}
