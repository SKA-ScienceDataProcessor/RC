#!/bin/bash

export CC=gcc
export CXX=g++

targets="X86;NVPTX"

mkdir -p llvm-build
cd llvm-build
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=$PWD/../llvm-install -DCMAKE_BUILD_TYPE=Release -DLLVM_INCLUDE_TESTS=OFF -DLLVM_TARGETS_TO_BUILD=$targets -DLLVM_BUILD_DYLIB=1 -DLLVM_ENABLE_TERMINFO=0 ../llvm-3.5.2.src || exit 1
make -j8 || exit 1
make install || exit 1

cd ..
