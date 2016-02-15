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

# -- GASnet --------------------------------------------------------------------
# Right now GASnet does not seem to be available in any modules on cluster.

# getting GASnet
git clone https://github.com/thesz/GASnet.git GASnet

# Build it for cluster architecture.
cd GASnet/GASNet-1.26.0
# XXX: figure out flags. Right now - UDP.
./configure
make
cd ../..

set GASNET_BIN = $ROOT/GASnet/GASNet-1.26.0/bin

# -- Terra ---------------------------------------------------------------------
# Terra also unavailable.

git clone https://github.com/zdevito/terra.git terra
cd terra
./configure
make
cd ..

set TERRA_BIN = $ROOT/terra/release/bin

TERRA = $TERRA_BIN/terra

# -- Legion --------------------------------------------------------------------
# We will build Legion by compiling one of the applications.

git clone https://github.com/thesz/legion.git Legion
cd Legion/language/examples

$TERRA circuit.rg
