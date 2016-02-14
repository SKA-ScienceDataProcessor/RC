#!/bin/sh

# Setting up cluster environment using *module* system.

module purge

# Cluster config.
module load default-wilkes

# Known to wotk with both GASnet and Legion.
module load gcc/4.8.1
