#!/bin/bash
nvcc -I../CPU_Gridders -I../Oskar_New_Reader --cubin -gencode arch=compute_35,code=sm_35 all.cu
