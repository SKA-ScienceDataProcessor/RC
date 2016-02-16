#!/bin/bash

# Setting up the environment to build everything - download packages, build them, etc.

# The root dir is a level above SKA-RC root.
export MS7_DIR="`pwd`/../.."
export ROOT="`pwd`/../../../.."

# The directory to have all builds in. Easier to clean.
export BUILDDIR="$ROOT/build"
mkdir -p $BUILDDIR

# Clean the environment.
./clean-env.sh

# Going to the root.
cd $BUILDDIR

# -- LLVM ----------------------------------------------------------------------
# LLVM will be our compiler of choice for everything.

# TODO: add a test for working LLVM in environment - cluster can have one sometimes.
wget -c http://llvm.org/releases/3.7.1/llvm-3.7.1.src.tar.xz || exit 1
tar xf llvm-3.7.1.src.tar.xz || exit 1

# Prevent CMake compplaints.
rm -rf cfe-3.7.1.src/CMakeFiles
rm -f CMakeCache.txt

wget -c http://llvm.org/releases/3.7.1/cfe-3.7.1.src.tar.xz || exit 1
wget -c http://llvm.org/releases/3.7.1/compiler-rt-3.7.1.src.tar.xz || exit 1
cd llvm-3.7.1.src/tools
tar xf ../../cfe-3.7.1.src.tar.xz
tar xf ../../compiler-rt-3.7.1.src.tar.xz
cd ../..

# Perform generation of Makefiles and then make and install the LLVM.
mkdir -p llvm-build
cd llvm-build
cmake -v -G "Unix Makefiles" -DCMAKE_BUILD_TYPE="Release" -DCMAKE_INSTALL_PREFIX="$BUILDDIR/llvm" -DLLVM_BUILD_LLVM_DYLIB="1" $BUILDDIR/llvm-3.7.1.src

make -j4 || exit 1
make install || exit 1

export LLVMBIN=$BUILDDIR/llvm/bin

export PATH=$LLVMBIN:$PATH

echo "LLVMBIN: $LLVMBIN"
echo "PATH: $PATH"

cd ..

# -- Terra ---------------------------------------------------------------------
# Terra also unavailable on cluster.

git clone https://github.com/zdevito/terra.git terra || (cd terra; git pull; cd ..) || exit 1

cd terra

echo "LLVM_CONFIG = $LLVMBIN/llvm-config" >Makefile.inc

make all || exit 1
cd ..

export TERRA_BIN=$BUILDDIR/terra/release/bin

export TERRA=$TERRA_BIN/terra

# -- Legion --------------------------------------------------------------------
# We will build Legion by compiling one of the applications.

git clone https://github.com/thesz/legion.git Legion

cd Legion/language/examples

$TERRA circuit.rg || exit 1

cd ../../..

# -- GASnet --------------------------------------------------------------------
# Right now GASnet does not seem to be available in any modules on cluster.

# getting GASnet.
# TODO: make git clone/pull choice a proc.
git clone https://github.com/thesz/GASnet.git GASnet

# Build it for cluster architecture.
cd GASnet/GASNet-1.26.0
# XXX: figure out flags. Right now - UDP.
./configure || exit 1
make || exit 1
cd ../..

set GASNET_BIN = $ROOT/GASnet/GASNet-1.26.0/bin

