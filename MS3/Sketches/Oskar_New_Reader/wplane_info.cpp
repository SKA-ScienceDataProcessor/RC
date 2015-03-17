/* 
  Copyright (C) 2015 Braam Research, LLC.
 */
#include <cstdio>
#include <cstring>

#define WPLANES 32
#define TIMESTEPS 200
#define BASELINES 32131

int slices[BASELINES];

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
    slices[bld.bl]++;
  }
}

int main(){
  for(int i=0; i < BASELINES; i++) slices[i] = 0;
  for(int i=0; i < WPLANES; i++) proc(i);
  for(int i=0; i < BASELINES; i++) {
    if (slices[i] > 1)
      printf("Baseline %2d is sliced between %d planes\n", i, slices[i]);
  }
}
