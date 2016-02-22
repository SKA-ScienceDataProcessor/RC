#!/bin/bash

mkdir -p build
cd build
gcc -v
export CC=gcc
export CXX=g++
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=$PWD/../llvm-install -DCMAKE_BUILD_TYPE=Release -DLLVM_INCLUDE_TESTS=OFF -DLLVM_TARGETS_TO_BUILD="X86;NVPTX" -DLLVM_BUILD_DYLIB=1 ../llvm-3.5.0.src
make -j4
make install

cd ..

