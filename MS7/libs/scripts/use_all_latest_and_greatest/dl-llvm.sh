#!/bin/bash

# Downloading and unpacking LLVM 3.5.2. This activity is the same for both cluster and local build.

wget -c http://llvm.org/releases/3.5.2/llvm-3.5.2.src.tar.xz || exit 1
wget -c http://llvm.org/releases/3.5.2/cfe-3.5.2.src.tar.xz || exit 1
wget -c http://llvm.org/releases/3.5.2/compiler-rt-3.5.2.src.tar.xz || exit 1

tar xf llvm-3.5.2.src.tar.xz

cd llvm-3.5.2.src/tools
tar xf ../../cfe-3.5.2.src.tar.xz
tar xf ../../compiler-rt-3.5.2.src.tar.xz
cd ../..
