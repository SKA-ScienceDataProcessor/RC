## Installation Steps:

### Installing GHC

Install ghc-7.8.3:

    $ wget http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-x86_64-unknown-linux-centos65.tar.bz2
    $ wget http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-x86_64-unknown-linux-centos65.tar.bz2  
    $ bzip2 -d ghc-7.8.3-x86_64-unknown-linux-centos65.tar.bz2
    $ tar -xvf ghc-7.8.3-x86_64-unknown-linux-centos65.tar
    $ cd ghc-7.8.3
    $ ./configure  --prefix=$HOME/opt
    $ make install

**NOTE:** Add the path of the ghci binary in the file `~/.bashrc, export PATH=$PATH:$HOME/opt/bin`

    $ ghci
    GHCi, version 7.6.3: [http://www.haskell.org/ghc/](http://www.haskell.org/ghc/)  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... <command line>: can't load .so/.DLL for: libgmp.so    (libgmp.so: cannot open shared object file: No such file or directory)

If this error persists, then as root user, perform the following steps:

    # cd /usr/lib64/
    # ln -s libgmp.so.3 libgmp.so
    # ghci
    [hpcsriv1@login-sand7 ddp-erlang-style]$ ghci
    GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Prelude>

As non-root user, perform following steps -

    $ cd $HOME/opt/lib
    $ ln -s /usr/lib64/libgmp.so.3 libgmp.so
    $ echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/opt/lib" > ~/.bashrc
    $ source ~/.bashrc
    $ ghci
    [hpcsriv1@login-sand7 ddp-erlang-style]$ ghci
    GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Prelude>

### Installing Cabal
Download `Cabal-1.18.1.3.tar.gz` from  `https://hackage.haskell.org/package/Cabal-1.18.1.3/`
[Cabal-1.18.1.3.tar.gz](https://hackage.haskell.org/package/Cabal-1.18.1.3/Cabal-1.18.1.3.tar.gz)

    $ tar -zxvf Cabal-1.18.1.3.tar.gz
  	$ cd Cabal-1.18.1.3
    $ runhaskell Setup configure --prefix=$HOME/opt
  	$ runhaskell Setup build

**NOTE:** May following error persist, if gmp-devel package is not installed.

	Building Cabal-1.18.1.3…
    Preprocessing library Cabal-1.18.1.3…
    /usr/bin/ld: cannot find -lgmp
    collect2: ld returned 1 exit status ]
  	$ runhaskell Setup install

Download `cabal-install-1.18.0.3.tar.gz` from

    $ wget https://hackage.haskell.org/package/cabal-install-1.18.0.3/cabal-install-1.18.0.3.tar.gz
	$ tar -zxvf cabal-install-1.18.0.3.tar.gz
	$ cd cabal-install-1.18.0.3
  	$ ./bootstrap.sh

**NOTE:** This will create a directory structure `~/.cabal/bin` under `$HOME`. Now, we add   `$HOME/.cabal/bin` to the `PATH`. In `~/.bashrc` , add this line -

    echo "export PATH=$PATH:$HOME/bin:$HOME/.cabal/bin" > ~/.bashrc
    source ~/.bashrc
    $ which cabal
    ~/.cabal/bin/cabal

Following directories are also present  - `lib `and `config`

### BUILDING SKA_MS1

    $ mkdir SKA_MS1
    $ cd SKA_MS1
    $ cabal sandbox init
    $ git clone [git@github.com](mailto:git@github.com):SKA-ScienceDataProcessor/RC.git
    $ cd RC/MS1/ddp-erlang-style/
    $ cabal configure
    $ cabal install --only-dependencies
    $ cabal build
    $ cabal install

This will throw error that LLVM missing, so we again download these dependencies:

    $ wget  http://llvm.org/releases/3.4/llvm-3.4.src.tar.gz
    $ wget http://llvm.org/releases/3.4/clang-3.4.src.tar.gz
    $ wget http://llvm.org/releases/3.4/compiler-rt-3.4.src.tar.gzsrc.tar.gz
    $ tar -zxvf llvm-3.4.src.tar.gz
    $ tar -zxvf clang-3.4.src.tar.gz
    $ tar -zxvf compiler-rt-3.4.src.tar.gz
    $ mv clang-3.4 clang
    $ mv clang llvm-3.4/tools/
    $ mv compiler-rt-3.4 compiler-rt
    $ mv compiler-rt llvm-3.4/projects/
    $ mkdir build
    $ cd build/
    $ ../llvm-3.4/configure --enable-shared

If everything is good we can go ahead and compile it and also keep track of the time taken. After compilation all binaries are available in the `build/Release+Asserts/bin` folder and libraries are available in the `build/Release+Asserts/lib` folder.

    $ time make -j 3

#### Verifying installation

We run some test suites that comes along with LLVM to verify everything is working

    $ make check-all


Now, we again add the path of the LLVM’s bin in `~/.bash_profile`

    PATH=$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/build/Release+Asserts/bin

And the LLVM’s lib path by creating soft link in `$HOME/opt/lib` with `build/Release+Asserts/lib`

Finally, we execute `cabal build` to build our binaries.

	  $ cabal install ghc-event

This install ghc-event tool needed to produce textual logs from binary log files.
