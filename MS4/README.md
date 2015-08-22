
MS4 Build Instructions
==

Prerequisites
--

You will need:

* A Linux system recent enough to have `perf-events`
* A CUDA installation (>= 6.0)
* An OpenMP-enabled FFTW3 installation

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

Check out the code and build:

     git clone https://github.com/SKA-ScienceDataProcessor/RC.git
     cd RC/MS4
     sh boot.sh
     cabal install

Running
--

Binaries should have been installed into `bin/`. Now change into the
`cfg/` directory and have a look at the configuration files:

     cd cfg
     $EDITOR imaging.cfg
     $EDITOR data.cfg

In `imaging.cfg` you can set the imaging pipeline parameters as well
as the kernels to use. Note that only certain configurations might
work.

In `data.cfg` you configure the data sets to use and how often they
should be repeated (for testing purposes).

If the configuration makes sense, you could simply start the imaging
pipeline locally at this point:

    ../bin/imaging-dataflow --nprocs 1

Make sure that both the configuration files as well as the required
data files are present. Note that running only one process you can
only have one data set in `data.cfg`.

For running on SLURM, make sure that one instance of
`imaging-dataflow` is started per node (say, using `mpirun`). The
program will need at least one node per data set in `data.cfg`. Make
sure that the working directory is set up so both configuration files
and data can be found, then just run the executable without
parameters:

    /absolute/path/to/RC/MS4/bin/imaging-dataflow

In either case, the resulting image will be called `output.img`. Note
that this is still an incomplete prototype at this point, so it is
quite likely that the output will not actually make much sense.

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

Which should generate a [profiling report](http://people.ds.cam.ac.uk/pw410/out/hpc/1634160-s.html).

