INC=$(HOME)/data/opt/include/oskar
## LIB=$(HOME)/data/opt/lib
CC=g++
## LDFLAGS=-L$(LIB) -loskar
INCFLAGS=-I$(INC) -I$(INC)/apps/log -I$(INC)/utility -I$(INC)/interferometry -I$(INC)/station -I$(INC)/element -I$(INC)/splines -I$(INC)/settings/struct
## CFLAGS=-O3 -Wall
CXXFLAGS=-O3 -Wall -std=c++14 $(INCFLAGS)

all: o2a.o

o2a.o: o2a.cpp o2a.h

clean:
	rm -f *.o
