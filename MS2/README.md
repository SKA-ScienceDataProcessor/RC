# Getting and building Halide on cluster

Halide team periodically builds [binary releases of Halide](https://github.com/halide/Halide/releases), which may be used locally on desktops provided sufficiently recent gcc (4.9.x) compiler is installed.

Wilkes cluster has no gcc-4.9.x installed yet, and installing it locally (taking into account the sheer number of dependencies) looks problematic. Hence, we choose to rebuild Halide against gcc-4.8.1, which can be used on cluster provided we load the corresponding environment using cluster's `module` command.

Also `clang` is required to build Halide. We didn't try to use binary releases from LLVM team because we need `clang` to be built against the same gcc version we use on cluster.

Thus, building `clang` on cluster looks as follows:

1. Execute `module load gcc/4.8.1` command. This command is **cluster-specific** and is necessary to set necessary environment variables to bring gcc-4.8.1 in scope.

2. Download and build clang/llvm the way [stated in official documentation](http://llvm.org/docs/GettingStarted.html#getting-started-quickly-a-summary). Take into account the following:
  * Use release branches instead of trunks or download release sources archives and unpack them manually. We tested building Halide against [3.4.2 release](http://llvm.org/releases/download.html#3.4.2).
  * No any special configuration options are required. It is good practice to specify `--prefix /place/to/put/clang` option though to point to well-defined location, so that `/place/to/put/clang/bin` could be added to `PATH` environment variable.
  * If using archived sources, please maintain directory layout described in the documentation (`clang` goes under `llvm/tools` subdirectory and `compiler-rt` goes to `llvm/projects` llvm subdirectory.
  * Please, limit the number of job slots for `make` command. `make -j8` goes pretty smooth.
  * To use thus built clang add `/place/to/put/clang/bin` to your `PATH`.

Currently, Halide gridding code assumes Halide distribution is placed on the same directory level as RC repository is and resides in `halide` directory. For example, to be able to build Halide gridding code with no problems please execute

`git clone https://github.com/SKA-ScienceDataProcessor/RC.git`

and

`git clone https://github.com/halide/Halide.git halide`

from the same directory. Note we renamed `Halide` to `halide` here. This way it is made compatible with our current build infrastructure and with possible (desktop/future cluster) usage of Halide binary releases which unpacks to `halide` directory.

Now go to `halide` directory and make it with the following:

`make -j8 CLANG="clang --gcc-toolchain=/usr/local/Cluster-Apps/gcc/4.8.1"`

This is required when Halide is built on cluster because clang has bug/feature (for ages) preventing it from picking correct gcc environment.
