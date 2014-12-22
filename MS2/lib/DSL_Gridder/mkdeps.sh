#!/bin/bash
ghc -M -hidir ./tmp -odir ./tmp -stubdir ./tmp -i.. -no-user-package-db -package-db ../../.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d -dep-suffix "" gridding-in-memory.hs
