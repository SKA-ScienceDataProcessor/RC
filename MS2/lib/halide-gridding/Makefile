HALIDE_PATH = ../../../../halide

HALIDE_BIN = $(HALIDE_PATH)/bin
HALIDE_LIB_PATH = $(HALIDE_BIN)
HALIDE_LIB = $(HALIDE_LIB_PATH)/libHalide.a

HALIDE_INCLUDE = $(HALIDE_PATH)/include
HALIDE_SUPPORT_INCLUDE = $(HALIDE_PATH)/apps/support

run_performance: test_performance
	HL_NUM_THREADS=8 ./test_performance
	HL_NUM_THREADS=4 ./test_performance

run_conformance: test_conformance
	./test_conformance

test_conformance: test_conformance.cpp griddingSimple_float.h griddingSimple_float.o
	g++ -O3 -o test_conformance -I$(HALIDE_INCLUDE) -I$(HALIDE_SUPPORT_INCLUDE) test_conformance.cpp griddingSimple_float.o $(HALIDE_LIB) -lpthread -ldl

test_performance: test_performance.cpp griddingSimple_float.h griddingSimple_float.o
	g++ -O3 -o test_performance -I$(HALIDE_INCLUDE) -I$(HALIDE_SUPPORT_INCLUDE) test_performance.cpp griddingSimple_float.o $(HALIDE_LIB) -lpthread -ldl

griddingSimple_float.h griddingSimple_float.o: gridding
	./gridding

reduced-example-compiled.c: reduced-example
	./reduced-example

gridding: gridding.cpp
	g++ -std=c++11 -o gridding gridding.cpp -I$(HALIDE_INCLUDE) -ldl -lpthread $(HALIDE_LIB) ${shell llvm-config --ldflags}

clean:
	rm -f gridding griddingSimple_* test_conformance

reduced-example: reduced-example.cpp
	g++ -o reduced-example reduced-example.cpp -I$(HALIDE_INCLUDE) -ldl -lpthread $(HALIDE_LIB) ${shell llvm-config --ldflags}
