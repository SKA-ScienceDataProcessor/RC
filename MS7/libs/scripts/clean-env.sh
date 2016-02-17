#!/bin/bash

# The packages we have to enable:
#  - CMake - for llvm
#  - gcc (4.8 works, 4.9 doesn't) - for llvm
#  - python (>=2.7, for Regent installation)
#  - autoconf, automake - for GASnet

if command -v module; then
	# Setting up cluster environment using *module* system.
	module purge

	# Cluster config.
	module load default-wilkes

	# Known to wotk with both GASnet and Legion.
	module load gcc/4.8.1
else
	echo "not on cluster. Please have gcc 4.8, CMake 3.2+, python 2.7+, autoconf and automake."
fi