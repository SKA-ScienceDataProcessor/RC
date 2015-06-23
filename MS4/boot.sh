#!/bin/sh

# Set up the sandbox
cabal sandbox init
cabal sandbox add-source dep/cupti
cabal sandbox add-source dep/linux-perf-stat

# Install dependencies into sandbox
cabal install --only-dependencies
