#!/bin/bash

# Make and test terra.

# Point to proper llvm-config.
#PATH=$PWD/llvm-install/bin:$PATH
#export LLVM_CONFIG=$PWD/llvm-install/bin/llvm-config

# download.
git clone https://github.com/zdevito/terra.git

# go into terra to build.
cd terra

make

cd tests
../terra run

cd ../..
