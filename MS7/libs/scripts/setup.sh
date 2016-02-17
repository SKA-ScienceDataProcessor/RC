#!/bin/bash

# Setting up the environment to build everything - download packages, build them, etc.

# Bolierplate reduction.
function clonepull {
    git clone $1 $2 || (cd $2 ; git pull) || exit 1
}

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

# -- GASnet --------------------------------------------------------------------
# Right now GASnet does not seem to be available in any modules on cluster.

# getting GASnet.
# TODO: make git clone/pull choice a proc.
clonepull https://github.com/thesz/GASnet.git GASnet

# Conduit selection. This variable also used in Legion build.
# TODO: move to clean-env.sh???
export CONDUIT=udp

# Build it for cluster architecture.
cd GASnet/GASNet-1.26.0

# Set configure switches based on conduit selected above.
set GASNET_CONFIGURE_OPTS=""
case $CONDUIT in
    udp) GASNET_CONFIGURE_OPTS="" ;;
    *) echo "invalid conduit $CONDUIT selected" ; exit 1 ;;
esac
./configure || exit 1
make || exit 1
cd ../..

export GASNET_BIN=$ROOT/GASnet/GASNet-1.26.0/bin

export GASNET=$ROOT/GASnet/GASNet-1.26.0

# -- LLVM ----------------------------------------------------------------------
# LLVM will be our compiler of choice for everything.

export LLVM_VER=3.5

# TODO: add a test for working LLVM in environment - cluster can have one sometimes.
wget -c http://llvm.org/releases/$LLVM_VER/llvm-$LLVM_VER.src.tar.xz || exit 1
tar xf llvm-$LLVM_VER.src.tar.xz || exit 1

# Prevent CMake compplaints.
rm -rf llvm-$LLVM_VER.src/CMakeFiles
rm -f CMakeCache.txt

wget -c http://llvm.org/releases/3.7.1/cfe-$LLVM_VER.src.tar.xz || exit 1
wget -c http://llvm.org/releases/3.7.1/compiler-rt-$LLVM_VER.src.tar.xz || exit 1
cd llvm-$LLVM_VER.src/tools
tar xf ../../cfe-$LLVM_VER.src.tar.xz
tar xf ../../compiler-rt-$LLVM_VER.src.tar.xz
cd ../..

# Perform generation of Makefiles and then make and install the LLVM.
mkdir -p llvm-build
cd llvm-build
cmake -v -G "Unix Makefiles" -DCMAKE_BUILD_TYPE="Release" -DCMAKE_INSTALL_PREFIX="$BUILDDIR/llvm" -DLLVM_BUILD_LLVM_DYLIB="1" $BUILDDIR/llvm-$LLVM_VER.src

make -j8 || exit 1
make install || exit 1

export LLVMBIN=$BUILDDIR/llvm/bin

export PATH=$LLVMBIN:$PATH

echo "LLVMBIN: $LLVMBIN"
echo "PATH: $PATH"

cd ..

# -- Terra ---------------------------------------------------------------------
# Terra also unavailable on cluster.

clonepull https://github.com/zdevito/terra.git terra

cd terra

echo "LLVM_CONFIG = $LLVMBIN/llvm-config" >Makefile.inc

make all || exit 1
cd ..

export TERRA_DIR=$BUILDDIR/terra

export TERRA_BIN=$BUILDDIR/terra/release/bin

export TERRA=$TERRA_BIN/terra

# -- Legion --------------------------------------------------------------------
# We will build Legion by compiling one of the applications.

clonepull https://github.com/thesz/legion.git Legion

# Go to Regent place.
cd Legion/language

# Running the installation, enabling the GASnet..
./install.py --with-terra=$TERRA_DIR/release --gasnet

# Run the tests.
echo "=========================================================="
echo "Running Regent tests."

./test.py -v

# Up from Regent.
cd $BUILDDIR

