/* 
  Copyright (C) 2015 Braam Research, LLC.
 */

#include <cstdio>

struct baseline_descr {
  int bl;
  int items;
};

int main(int argc, char * argv[]) {

  if (argc < 2) {
    printf("usage: %s <wplane file>\n", argv[0]);
  }

  FILE * f = fopen(argv[1], "rb");
  if (f == nullptr) {
    printf("Error opening %s\n", argv[1]);
    return -1;
  }

  fseek(f, -sizeof(int), SEEK_END);
  int num_baselines;
  fread(&num_baselines, sizeof(int), 1, f);
  printf("File %s contains info for %d baselines:\n", argv[1], num_baselines);
  fseek(f, -(sizeof(int) + sizeof(baseline_descr) * num_baselines), SEEK_END);

  for(int i=0; i < num_baselines; i++) {
    baseline_descr bld;
    fread(&bld, sizeof(baseline_descr), 1, f);
    const char * tmark;
    if (bld.items != 200) tmark = " TRANS";
    else tmark = "";
    printf("%8d %8d%s\n", bld.bl, bld.items, tmark);
  }
}
