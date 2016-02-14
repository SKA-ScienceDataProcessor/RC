#!/bin/sh

# Setting up the environment to build everything - download packages, build them, etc.

# The root dir is a level above SKA-RC root.
MS7_DIR = `pwd`/../..
ROOT = `pwd`/../../../..

# Clean the environment.
./clean.sh

# Going to the root.
cd $ROOT

# -- GASnet --------------------------------------------------------------------
# Right now GASnet does not seem to be available in any modules on cluster.

# getting GASnet
git clone https://github.com/thesz/GASnet.git GASnet

# Build it for cluster architecture.
cd GASnet
# XXX: figure out flags. Right now - UDP.
./configure
make
cd ..

GASNET_BIN = $ROOT/GASnet/bin

# -- Terra ---------------------------------------------------------------------
# Terra also unavailable.

git clone https://github.com/zdevito/terra.git terra
cd terra
./configure
make || die "terra build failed."
cd ..

TERRA_BIN = $ROOT/terra/release/bin

TERRA = $TERRA_BIN/terra

# -- Legion --------------------------------------------------------------------
# We will build Legion by compiling one of the applications.

git clone https://github.com/thesz/legion.git Legion
cd Legion/language/examples

$TERRA circuit.rg
