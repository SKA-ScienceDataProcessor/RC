#!/bin/sh

# Set up the sandbox
cabal sandbox init
cabal sandbox add-source cupti
cabal sandbox add-source linux-perf-stat

# Install dependencies into sandbox
cabal install --only-dependencies

# Create link to where binaries reside
ln -s .cabal-sandbox/bin .
