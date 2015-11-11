#!/bin/bash
export HALIDE_DIR=/home/awson/data/Work/HalideBuild/halide
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HALIDE_DIR/bin
g++ -std=c++11 -O2 -Wall -I$HALIDE_DIR/include -o generate_all_halide generate_all_halide.cpp scatter_halide.cpp -L$HALIDE_DIR/bin -lHalide
./generate_all_halide kernels_halide
g++ -std=c++11 -O2 -Wall -fopenmp -o cppcycle cppcycle.cpp kernels_halide.obj -ldl
