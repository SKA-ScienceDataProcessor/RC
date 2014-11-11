-- |Setup.hs
--
-- A custom build script for dna programs.
--
-- Copyright (C) 2014 Braam Research, LLC.

module Main(main) where

import Distribution.Simple
import Distribution.Simple.Configure (configure)
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

-- |Change preconfigure hooks - build Halide objects, etc.
dnaProgramsUserHooks = simpleUserHooks {
          confHook = preConfHalideConfigureBuild
        }

-- |Check for Halide and update configuration for building C++ files.
preConfHalideConfigureBuild :: (GenericPackageDescription, HookedBuildInfo)
            -> ConfigFlags -> IO LocalBuildInfo
preConfHalideConfigureBuild (packageDescr, buildInfo) configFlags = do
        localBuildInfo <- configure (packageDescr, buildInfo) configFlags
{-
        putStrLn $ unlines
                [ show packageDescr
                , show buildInfo
                , show configFlags
                , show localBuildInfo
                ]
-}
        return localBuildInfo

-- |We slightly change behaviour of cabal pipeline to accomodate for Halide.
main = defaultMainWithHooks dnaProgramsUserHooks
