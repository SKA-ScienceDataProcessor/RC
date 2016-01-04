
MS5 Build Instructions
==

Prerequisites
--

You will need:

* a Linux kernel recent enough to support the `perf-events` interface.

* A recent enough version of GCC that it supports `std=c++11`.

* Halide (http://halide-lang.org/) for generating the kernels, and
  LLVM >= 3.5 (http://llvm.org/) by transitivity. Make sure include
  and library files are in the search path! This might require setting
  `CPATH`, `LIBRARY_PATH` as well as `LD_LIBRARY_PATH`.


Building
--

You will need a recent version of GHC installed, we are targetting
7.10 currently. Follow the instructions on the Haskell home page
(https://www.haskell.org/downloads/linux) on how to install it. You
will only need GHC and cabal-install to get this prototype to work, so
you can skip Stack if you want.

Check out the code and build:

     git clone https://github.com/SKA-ScienceDataProcessor/RC.git
     cd RC/MS5
     sh boot.sh
     cabal install

Binaries should have been installed into `bin/`. You will need need
input data, which members of the SDP project can grab via Confluence
(https://confluence.ska-sdp.org/display/DSL/Reference+Results). The
prototype code assumes that `test_p00_s00_f00.vis` and `gcf16.dat` can
be found in the working directory of the program.

To run the `gridding` program locally, execute it as follows:

     bin/gridding --nprocs 4

To run it using SLURM, simply make sure that the program is executed
on every single node:

     bin/gridding

In either case the execution will produce an image `out.img` and a
profile that can be visualised as explained in MS4.

Benchmarks
--

For this milestone we have a number of benchmarks that test the
performance of kernel implementations in isolation. In contrast to the
DNA programs the benchmarks do not use Oskar visibilitity data, but
instead expect a raw `vis.dat` file in the working directory. It can
be found on Confluence in the same place as the Oskar file.

The following benchmarks are available:

* `bench/cppcycle.cpp`: AOT Halide gridding kernels
* `bench/jit/cppcycle.cpp`: JIT Halide gridding kernels
* `bench/jit_gpu/cppcycle.cpp`: JIT Halide gridding kernels (for GPU)
* `bench/jit_gridder_degridder/cppcyclej.cpp`: JIT Halide degridding kernels
* `bench/cuni/cppcycle.cpp`: C++ gridding kernels

Many of the benchmark programs test a large number of kernel
configurations. Refer to the source code.

Use the `b.sh` script for building the benchmarks, or otherwise simply
use the following command line:

    g++ *.cpp -lHalide --std=c++11 -lgomp -ocppcycle -fopenmp -O2
    ./cppcycle

