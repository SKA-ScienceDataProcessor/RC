#!/bin/bash
g++ -I../cmisc -std=c++11 -O3 -ffast-math -fomit-frame-pointer -fopenmp -mavx -o cppcyclegmp cppcycle.cpp scatter_gridder_fixed_gcf_5t_vis.cpp ../cmisc/aligned_malloc.cpp
