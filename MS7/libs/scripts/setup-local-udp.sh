#!/bin/bash

# Setting up the environment to build everything - download packages, build them, etc.

# Bolierplate reduction.
function clonepull {
    git clone $1 $2 || (cd $2 ; git pull) || exit 1
}

# Location of script files.
export SCRIPT_DIR=`pwd`

# The root dir is a level above SKA-RC root.
export MS7_DIR="`pwd`/../.."
export ROOT="`pwd`/../../../.."

# The directory to have all builds in. Easier to clean.
export BUILDDIR="$ROOT/build"
mkdir -p $BUILDDIR

# Going to the root.
cd $BUILDDIR

# -- LLVM ----------------------------------------------------------------------
# Download and build LLVM.

# Download LLVM.
$SCRIPT_DIR/dl-llvm.sh || exit 1

# Build LLVM.
$SCRIPT_DIR/mk-llvm.sh || exit 1

# Provide one true LLVM.
export PATH=$PWD/llvm-install/bin:$PATH
export LLVM_CONFIG=$PWD/llvm-install/bin/llvm-config

# -- GASnet --------------------------------------------------------------------
# Right now GASnet does not seem to be available in any modules on cluster.

cd $BUILDDIR

# getting GASnet.
clonepull https://github.com/SKA-ScienceDataProcessor/gasnet.git gasnet

# Build it for default config - MPI and UDP, nothing else.
cd gasnet
make
cd ..

export GASNET_ROOT="$PWD/gasnet/release"
export GASNET_BIN="$GASNET_ROOT/bin"

# -- Terra ---------------------------------------------------------------------
# Terra also unavailable on cluster.

clonepull https://github.com/zdevito/terra.git terra

cd terra

make all || exit 1
cd ..

export TERRA_DIR=$BUILDDIR/terra/release

# -- Legion --------------------------------------------------------------------
# We will build Legion by compiling one of the applications.

clonepull https://github.com/thesz/legion.git Legion

# Go to Regent place.
cd Legion/language

# Running the installation, enabling the GASnet.
# TODO: optionally enable CUDA.
CONDUIT=udp ./install.py --with-terra=$TERRA_DIR --gasnet || exit 1

$GASNET_BIN/amudprun -n 2 -spawn L ./regent.py examples/circuit.rg

# Up from Regent.
cd $BUILDDIR

