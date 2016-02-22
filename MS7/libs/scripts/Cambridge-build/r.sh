#!/bin/bash

echo "Please, verify that you have performed the following commands:"
echo "   module purge"
echo "   module load default-wilkes"
echo "   module switch cuda/5.5 cuda/6.0"
echo "   module load gcc/4.9.2"

export CC=gcc
export CXX=g++

$CC -v
$CXX -v

# download and extract LLVM
#./dl-llvm.sh

# build and install LLVM
./mk-llvm.sh

# Provide one true LLVM.
export PATH=$PWD/llvm-install/bin:$PATH
export LLVM_CONFIG=$PWD/llvm-install/bin/llvm-config

# Build terra against true gcc and true LLVM.
#./mk-test-terra.sh

export TERRA_DIR=$PWD/terra/release

unset LG_RT_DIR
mkdir -p testlegion
cd testlegion
git clone https://github.com/StanfordLegion/gasnet.git
cd gasnet
# TODO: use IB-specific config!
make
cd ..
git clone -b master https://github.com/StanfordLegion/legion.git
cd legion/language
GASNET_ROOT="$PWD/../../gasnet/release" CONDUIT=udp ./install.py --gasnet --with-terra=$TERRA_DIR
LAUNCHER="$PWD/../../gasnet/release/bin/amudprun -n 1 -spawn L" ./regent.py examples/circuit.rg
