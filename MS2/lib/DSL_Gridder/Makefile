OPT=$(HOME)/data/opt
INC=$(OPT)/include/oskar
LIB=$(OPT)/lib
CC=g++
INCFLAGS=-Io2a -I$(INC) -I$(INC)/apps/log -I$(INC)/utility -I$(INC)/interferometry -I$(INC)/station -I$(INC)/element -I$(INC)/splines -I$(INC)/settings/struct
CXXFLAGS=-O3 -fPIC -Wall -std=c++14 $(INCFLAGS)
METRICS=-DSUPPORT_U=64 -DSUPPORT_V=64 -DTIMESTEPS=10 -DBLOCKS=36 -DNR_STATIONS=30 -DCHANNELS=1

CUDA_LIB_PATH=$(HOME)/data/cuda65/lib64
HALIDE_PATH=$(OPT)
HALIDE_INCLUDE=$(HALIDE_PATH)/include
HALIDE_BIN = $(HALIDE_PATH)/bin
HALIDE_LIB_PATH = $(HALIDE_BIN)
HALIDE_LIB = $(HALIDE_LIB_PATH)/libHalide.a

GHC_OPTS=-O2 -fPIC -Wall -threaded -hidir ./tmp -odir ./tmp -stubdir ./tmp -i.. -no-user-package-db -package-db ../../.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d

dslgridder: gridding-in-memory.hs DGridding.hs GriddersFFI.hs Oskar.hs libauxffi.so
	ghc $(GHC_OPTS) --make gridding-in-memory.hs -L. -lauxffi -L$(LIB) -loskar

libauxffi.so: romein_client.o halide_client.o writeOSKARc.o o2a.o OskarFFIprim.o GriddersFFIprim.o griddingSimple_double_CUDA.o
	g++ -shared -o libauxffi.so romein_client.o halide_client.o writeOSKARc.o o2a.o OskarFFIprim.o GriddersFFIprim.o griddingSimple_double_CUDA.o -L$(CUDA_LIB_PATH) -lcudart

GriddersFFIprim.o: GriddersFFIprim.cmm
	ghc -O2 -fPIC -c GriddersFFIprim.cmm

OskarFFIprim.o: OskarFFIprim.cmm
	ghc -O2 -fPIC -c OskarFFIprim.cmm

romein_client.o: romein_client.cc
	nvcc -Xptxas -v  -x cu --compile -ccbin=g++ -g -D__CUDA__ -I. $(METRICS) -use_fast_math -arch=compute_35 -code=compute_35 -Xcompiler "-O3,-fopenmp,-fPIC" romein_client.cc -o romein_client.o

romein_client.cc:
	wget --no-check-certificate https://github.com/awson/Romein-gridding/raw/doubles/Gridding-0.2/Gridding.cc && mv Gridding.cc romein_client.cc

griddingSimple_double_CUDA.h griddingSimple_double_CUDA.o: gridding
	./gridding

gridding: gridding.cpp
	g++ -std=c++11 -o gridding gridding.cpp -DWILKES_CLUSTER -I$(HALIDE_INCLUDE) $(HALIDE_LIB) -ldl -lpthread -lz

halide_client.o: halide_client.cpp griddingSimple_double_CUDA.h
	g++ -std=c++11 -fPIC -O2 -DWILKES_CLUSTER -I$(HALIDE_INCLUDE) -I. $(METRICS) -o halide_client.o -c halide_client.cpp

writeOSKARc.o: writeOSKARc.cpp writeOSKARc.h o2a/o2a.h
o2a.o: o2a/o2a.cpp o2a/o2a.h
	$(CC) $(CXXFLAGS) -c -o o2a.o o2a/o2a.cpp

clean:
	rm -f *.so *.o gridding gridding.html griddingSimple_* dslgridder && rm -f -R tmp
