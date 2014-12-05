INC=$(HOME)/data/opt/include/oskar
LIB=$(HOME)/data/opt/lib
CC=g++
LDFLAGS=-L$(LIB) -loskar
INCFLAGS=-I$(INC) -I$(INC)/apps/log -I$(INC)/utility -I$(INC)/interferometry -I$(INC)/station -I$(INC)/element -I$(INC)/splines -I$(INC)/settings/struct
CFLAGS=-O3 -Wall
CXXFLAGS=-O3 -Wall -std=c++14 $(INCFLAGS)

#test: uvw2romein
#	./uvw2romein ../run_remote/0-0.vis

all: uvw2romein amp2romein libreadOSKAR.a

uvw2romein: uvw2romein.o o2a.o
	$(CC) -o uvw2romein uvw2romein.o o2a.o $(LDFLAGS)

amp2romein: amp2romein.o o2a.o
	$(CC) -o amp2romein amp2romein.o o2a.o $(LDFLAGS)

libreadOSKAR.a: o2a.o readOSKAR.o

readOSKAR.o: readOSKAR.cpp readOSKAR.h o2a.h
uvw2romein.o: uvw2romein.c o2a.h
amp2romein.o: amp2romein.c o2a.h
o2a.o: o2a.cpp o2a.h

clean:
	rm -f *.o *.a amp2romein uvw2romein
