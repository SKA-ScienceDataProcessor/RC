#!/bin/bash

export CC=gcc
export CXX=g++

# Default targets are X86 (x68 and x86_64) only.
targets="X86"

# Check if we have CUDA.
if command -v nvcc 2>/dev/null ; then
    targets="X86;NVPTX"
fi

mkdir -p llvm-build
cd llvm-build
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=$PWD/../llvm-install -DCMAKE_BUILD_TYPE=Release -DLLVM_INCLUDE_TESTS=OFF -DLLVM_TARGETS_TO_BUILD=$targets -DLLVM_BUILD_DYLIB=1 ../llvm-3.5.0.src || exit 1
make -j8 || exit 1
make install || exit 1

cd ..