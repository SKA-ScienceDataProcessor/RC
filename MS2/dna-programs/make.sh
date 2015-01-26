#!/bin/sh

set -e

nvcc -arch=sm_21 -c cuda-dotp.cu -o cuda-dotp.o \
    -Xcompiler -fPIC


ghc --make -O2 \
    -no-user-package-db \
    -package-db "../.cabal-sandbox/$(uname -m)-linux-ghc-$(ghc -V | sed 's/[^0-9]*//')-packages.conf.d/" \
    -eventlog \
    ddp-in-memory-cuda.hs \
    -lcuda -lcudart || :
ghc --make -O2 \
    -no-user-package-db \
    -package-db "../.cabal-sandbox/$(uname -m)-linux-ghc-$(ghc -V | sed 's/[^0-9]*//')-packages.conf.d/" \
    -eventlog \
    ddp-in-memory-cuda.hs \
    -lcuda -lcudart cuda-dotp.o
