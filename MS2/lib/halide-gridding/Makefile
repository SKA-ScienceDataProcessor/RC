HALIDE_PATH = ../../../../halide

HALIDE_BIN = $(HALIDE_PATH)/bin
HALIDE_LIB_PATH = $(HALIDE_BIN)
HALIDE_LIB = $(HALIDE_LIB_PATH)/libHalide.a

HALIDE_INCLUDE = $(HALIDE_PATH)/include

gridding_compiled.cpp: gridding
	./gridding

reduced-example-compiled.c: reduced-example
	./reduced-example

gridding: gridding.cpp
	g++ -o gridding gridding.cpp -I$(HALIDE_INCLUDE) -ldl -lpthread $(HALIDE_LIB) ${shell llvm-config --ldflags}

clean:
	rm -f gridding gridding_compiled*

reduced-example: reduced-example.cpp
	g++ -o reduced-example reduced-example.cpp -I$(HALIDE_INCLUDE) -ldl -lpthread $(HALIDE_LIB) ${shell llvm-config --ldflags}
