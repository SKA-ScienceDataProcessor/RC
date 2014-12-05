INC=$(HOME)/data/opt/include/oskar
LIB=$(HOME)/data/opt/lib
CC=g++
LDFLAGS=-L$(LIB) -loskar
INCFLAGS=-I$(INC) -I$(INC)/apps/log -I$(INC)/utility -I$(INC)/interferometry -I$(INC)/station -I$(INC)/element -I$(INC)/splines -I$(INC)/settings/struct
CFLAGS=-O3 -Wall
CXXFLAGS=-O3 -Wall -std=c++14 $(INCFLAGS)

#test: uvw2romein
#	./uvw2romein ../run_remote/0-0.vis

uvw2romein: uvw2romein.o o2a.o
	$(CC) -o uvw2romein uvw2romein.o o2a.o $(LDFLAGS)

uvw2romein.o: uvw2romein.c
o2a.o: o2a.cpp

clean:
	rm -f o2a.o uvw2romein.o uvw2romein
