INC=$(HOME)/data/opt/include/oskar
LIB=$(HOME)/data/opt/lib
CC=g++
INCFLAGS=-I.. -I$(INC) -I$(INC)/apps/log -I$(INC)/utility -I$(INC)/interferometry -I$(INC)/station -I$(INC)/element -I$(INC)/splines -I$(INC)/settings/struct
CXXFLAGS=-O3 -Wall -std=c++14 $(INCFLAGS)

all: writeOSKAR

writeOSKAR: writeOSKAR.hs OskarFFI.hs OskarFFIprim.o writeOSKARc.o o2a.o
	ghc -O2 --make writeOSKAR.hs OskarFFI.hs OskarFFIprim.o writeOSKARc.o o2a.o -L$(LIB) -loskar

OskarFFIprim.o: OskarFFIprim.cmm
	ghc -O2 -c OskarFFIprim.cmm

writeOSKARc.o: writeOSKARc.cpp writeOSKARc.h ../o2a.h
o2a.o: ../o2a.cpp ../o2a.h
	$(CC) $(CXXFLAGS) -c -o o2a.o ../o2a.cpp

clean:
	rm -f *.o *.hi writeOSKAR
    