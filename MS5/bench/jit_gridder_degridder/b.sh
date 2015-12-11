#!/bin/bash
export HALIDE_DIR=/home/awson/data/Work/HalideBuild/halide
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HALIDE_DIR/bin
gcc -I. -I.. -I../../kernel/cpu/gridding -I$HALIDE_DIR/include -Wall -std=gnu++11 -O2 cppcyclej.cpp scatter_halide.cpp ../../kernel/cpu/gridding/degrid_halide.cpp -o bench.exe -L$HALIDE_DIR/bin -lHalide
