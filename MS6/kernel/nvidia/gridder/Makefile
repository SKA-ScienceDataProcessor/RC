ifeq ($(WILKES),1)
	ARCH = -arch=sm_35
endif

all: test testc

clean:
	rm *.o libGPUGrid.a test testc

grid_gpu.o : grid_gpu.cu Defines.h grid_gpu.cuh
	nvcc -std=c++11 $(ARCH) -c grid_gpu.cu -o grid_gpu.o

grid_gpuc.o : grid_gpuc.cu grid_gpu.cu Defines.h grid_gpu.cuh
	nvcc -std=c++11 $(ARCH) -c grid_gpuc.cu -o grid_gpuc.o

binsort.o : binsort.cpp grid_gpu.cuh Defines.h
	gcc -std=c++11 -c binsort.cpp -o binsort.o

halide_api.o: halide_api.cu grid_gpu.cuh
	nvcc -I../../common -std=c++11 $(ARCH) -c halide_api.cu -o halide_api.o

libGPUGrid.a: grid_gpu.o grid_gpuc.o binsort.o halide_api.o
	ar cr libGPUGrid.a grid_gpu.o grid_gpuc.o binsort.o halide_api.o

test : test.cu grid_gpu.cuh Defines.h libGPUGrid.a
	nvcc -std=c++11 $(ARCH) -o test test.cu -L. -lGPUGrid

testc : test.cu grid_gpu.cuh Defines.h libGPUGrid.a
	nvcc -std=c++11 $(ARCH) -D__COMBINED -o testc test.cu -L. -lGPUGrid
