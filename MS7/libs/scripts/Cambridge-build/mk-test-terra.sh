#!/bin/bash

# Make and test terra.

# Point to proper llvm-config.
#PATH=$PWD/llvm-install/bin:$PATH
#export LLVM_CONFIG=$PWD/llvm-install/bin/llvm-config

# download.
git clone https://github.com/zdevito/terra.git

#git checkout tags/release-2016-02-26

# go into terra to build.
cd terra

git checkout c501af43915

make

cd tests
../terra run

cd ../..
