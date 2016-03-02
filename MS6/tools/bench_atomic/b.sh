#!/bin/bash
g++ -std=c++11 -fno-rtti -O2 -o generate_all_halide generate_all_halide.cpp scatter_halide.cpp -lHalide -lpthread -ldl -lz
generate_all_halide kernels_halide
g++ -std=c++11 -fno-rtti -O2 -o cppcycle cppcycle.cpp kernels_halide_gpu.obj -lHalide -ldl -lrt
