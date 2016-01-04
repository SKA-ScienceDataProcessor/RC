#!/bin/sh

set -e
# Set up the sandbox
cabal sandbox init
cabal sandbox add-source dna/cupti
cabal sandbox add-source dna/linux-perf-stat
cabal sandbox add-source dna/core
cabal sandbox add-source dna/flow
cabal sandbox add-source kernel/oskar

# Install dependencies into sandbox
cabal install --only-dependencies

# Create link to where binaries reside
ln -s .cabal-sandbox/bin .
