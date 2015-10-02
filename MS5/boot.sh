#!/bin/sh

set -e
# Set up the sandbox
cabal sandbox init
cabal sandbox add-source ../MS4/dep/cupti
cabal sandbox add-source ../MS4/dep/linux-perf-stat
cabal sandbox add-source ../MS4/dep/oskar
cabal sandbox add-source dna
cabal sandbox add-source dna-flow

# Install dependencies into sandbox
cabal install --only-dependencies

# Create link to where binaries reside
ln -s .cabal-sandbox/bin .
