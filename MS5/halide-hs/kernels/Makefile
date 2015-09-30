CXX         = g++
CXXFLAGS    = -g -Wall -fno-strict-aliasing -std=c++11 -I$(HOME)/opt/include $(shell llvm-config-3.6 --cppflags)
LDFLAGS     = -L$(HOME)/opt/lib -lHalide $(shell llvm-config-3.6 --ldflags) -lLLVM-3.6
LDFLAGS_RUN = -ldl -lpthread
#
################################################################
.PHONY: all clean

all : \
	generate_f dotp \
	test_run
clean :
	rm -fv *.o a.out generate_f dotp sum

generate_f : generate_f.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $< -o $@
dotp : dotp.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $< -o $@

%.o : %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

################################################################
# Halide
kern_generate_f.o : generate_f
	./generate_f
kern_dotp.o : dotp
	./dotp
kern_sum.o : sum
	./sum

test_run : test_run.cpp kern_generate_f.o kern_dotp.o kern_sum.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS_RUN) -o $@ $^
