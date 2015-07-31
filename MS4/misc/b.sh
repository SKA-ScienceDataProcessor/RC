#!/bin/bash
export SRC=../kernel/cpu
## We use Haskell 'oskar' package but we use no Haskell code at all 
## and link to C/C++ part of Oskar Reader library only.
## To be fully independent from GHC installation compile MS4/dep/oskar C/C++ part separately.
export LINK_OSKAR="-L../.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/oskar-0.1.0.0 -lHSoskar-0.1.0.0"
g++ -I$SRC/../../dep/oskar -I$SRC/../../dep/oskar/oskar_binary -I$SRC/../common -std=gnu++11 -mavx -ffast-math -fopenmp -Wall -O3 -fomit-frame-pointer -o cppcycle cppcycle.cpp stats_n_utils.cpp $SRC/gcf/GCF.cpp $SRC/fft/fft_dyn_padded.cpp $SRC/herm/herm_padded.cpp $SRC/scatter_grid/scatter_gridder_w_dependent_dyn_1p.cpp $SRC/hogbom/hogbom.cpp -lfftw3 -lfftw3_omp $LINK_OSKAR
