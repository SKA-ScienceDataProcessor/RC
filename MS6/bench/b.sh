#!/bin/bash
export HALIDE_LOC=$HOME/data/Work/HalideBuild/halide/bin

export LD_LIBRARY_PATH=$HALIDE_LOC:$LD_LIBRARY_PATH
export HALIDE_OPTS="-I$HALIDE_LOC/../include -L$HALIDE_LOC"
g++ $HALIDE_OPTS -Wall -std=c++11 -O2 -o generate_all_halide generate_all_halide.cpp scatter_halide.cpp -lHalide
./generate_all_halide kernels_halide
g++ $HALIDE_OPTS -Wall -std=c++11 -O2 -o bin_gridder bin_gridder.cpp kernels_halide_gpu.o -lHalide -ldl -lpthread
