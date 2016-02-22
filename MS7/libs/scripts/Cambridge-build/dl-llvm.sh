#!/bin/bash


wget -c http://llvm.org/releases/3.5.0/llvm-3.5.0.src.tar.xz || exit 1
wget -c http://llvm.org/releases/3.5.0/cfe-3.5.0.src.tar.xz || exit 1
wget -c http://llvm.org/releases/3.5.0/compiler-rt-3.5.0.src.tar.xz || exit 1

tar xf llvm-3.5.0.src.tar.xz

cd llvm-3.5.0.src/tools
tar xf ../../cfe-3.5.0.src.tar.xz
tar xf ../../compiler-rt-3.5.0.src.tar.xz
cd ../..

