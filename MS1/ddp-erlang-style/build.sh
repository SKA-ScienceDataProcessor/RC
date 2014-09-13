#!/bin/sh

# it is quick enough to go without make.
gcc -Wall -std=c99 -o create-floats create-floats.c || exit 1

# building our package.
(cabal configure && cabal build) || exit 1
