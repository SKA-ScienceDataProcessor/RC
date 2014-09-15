How to try this: 

obtain a binary Haskell package from `http://www.haskell.org/ghc/dist/7.8.3/`

     configure —prefix $HOME/opt 
     make install

obtain LLVM source package from `http://llvm.org/releases/download.html`

     configure —prefix $HOME/opt
     make install

obtain cabal-install from `http://www.haskell.org/cabal/download.html`

     install the binary in your path

check out Radio Cortex

     git clone https://github.com/SKA-ScienceDataProcessor/RC.git
     cd RC/MS1/ddp-erlang-style
     cabal sandbox init
     cabal update
     cabal install --only-dependencies
     cabal install ghc-events
     cabal configure
     cabal build
     gcc -o create-floats  create-floats.c

A test run can be done locally

     sh test.sh
     test2.sh is virtually equal to an sbatch file
     
A visualization is created with: 

    use ghc-events to change binary event files into text files
    python logs2plot.py dir-with-events-txt-files

For more information see:  http://ska-sciencedataprocessor.github.io/RC/
