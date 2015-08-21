
MS3 Build Instructions
==

Prerequisites
--

You will need:

* A Linux system recent enough to have `perf-events`
* A CUDA installation (>= 6.0)
* A CMake installation for building `oskar_binary`

Getting Haskell
--

Quickest way to get going is to install the Haskell Plattform
(http://haskell.org/platform/). But you can also obtain a binary
Haskell package from http://www.haskell.org/ghc/dist/7.8.3/

     configure --prefix $HOME/opt
     make install

You will also need `cabal-install` from http://www.haskell.org/cabal/download.html:

     install the binary in your path

Building
--

For building you first need an installation of `oskar_binary`, which
you might have to request from the OSKAR development team
(http://www.oerc.ox.ac.uk/~ska/oskar2/):

    unzip oskar_binary.zip
    cd oskar_binary
    cmake .
    make

Then check out the code and build:

     git clone https://github.com/SKA-ScienceDataProcessor/RC.git
     cd RC/MS3
     sh boot.sh
     cabal install

Running DDP
--

A number of programs should have been installed into `bin/`. For
example, a number of different distributed dot product versions can be
started using e.g.:

    bin/ddp-in-memory --nprocs <N>
    bin/ddp-in-memory-collector --nprocs <N>
    bin/ddp-in-memory-hierarchical --nprocs <N>

Replace "<N>" with the number of cores you would like to use. Note
that the "hierarchical" version is about testng hierarchical failure
propagation, so it is expected for the program to fail.

Running Gridder
--

The gridder is meant to be run as a Cluster application in a SLURM
environment. It expects OSKAR visibility files with names of the form
`test_p%02d_s%02d_f%02d.vis` to be present in the working
directory. It will require 21 nodes to run through.

The full command line should be as follows:

    /absolute/path/to/RC/MS3/bin/Gridder_DNA

Visualisation
--

The imaging program generates automatically generates profiling
information into `$HOME/_dna`. For generating a profile overview first
make sure to install the command line `ghc-events` profile reader:

    cabal install ghc-events

Then switch to the `visualize` sub-directory for MS4 and invoke the
generator for the run, for example:

    cd visualize
    python html_plotter.py timeline ~/_dna/1634160-s 1634160-s.html

Which should generate a profiling report.

For more information see:  http://ska-sciencedataprocessor.github.io/RC/
