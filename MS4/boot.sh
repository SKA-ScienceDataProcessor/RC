#!/bin/sh

# Set up the sandbox
cabal sandbox init
cabal sandbox add-source dep/cupti
cabal sandbox add-source dep/linux-perf-stat
cabal sandbox add-source dep/oskar

# Install dependencies into sandbox
cabal install --only-dependencies

# Create link to where binaries reside
ln -s .cabal-sandbox/bin .
