-- |Setup.hs
--
-- A custom build script for dna programs.
--
-- Copyright (C) 2014 Braam Research, LLC.

module Main(main) where

import Control.Applicative
import Control.Monad

import Distribution.Simple
import Distribution.Simple.Configure (configure)
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Find
import Distribution.Simple.Program.Types
import Distribution.Simple.Utils (die)

import Distribution.System

import Distribution.PackageDescription

import Distribution.Text
import Distribution.ParseUtils
import Distribution.Compat.ReadP

import Data.Maybe
import Data.Char
import Data.List ( intersect )
import Debug.Trace

import System.Process
import System.FilePath

-- |Change preconfigure hooks - build Halide objects, etc.
dnaProgramsUserHooks = simpleUserHooks {
          confHook = preConfHalideConfigureBuild,
          -- NB: The following allows us to override NVCC location and
          -- options from the command line. The syntax is slightly
          -- non-obious:
          --
          --  $ cabal configure -- --with-nvcc=[...] --nvcc-options=[...]
          --
          -- Note the extra "--".
          hookedPrograms = nvccProgram : hookedPrograms simpleUserHooks,
          buildHook = cudaBuildHook
        }

nvccProgram :: Program
nvccProgram = (simpleProgram "nvcc")
              { programFindVersion  = findProgramVersion "--version" verSel }
 where verSel ""     = ""
       verSel output = tail $ last $ words $ last $ lines output

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

cudaBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks ->
                 BuildFlags -> IO ()
cudaBuildHook package lbi hooks flags = do

  -- Find all CUDA sources in libraries & executables. Update
  -- build information accordingly.
  library' <- case library package of
    Just lib -> do bi' <- cudaBuild lbi flags (buildDir lbi) (libBuildInfo lib)
                   return $ Just lib { libBuildInfo = bi' }
    Nothing  -> return Nothing

  -- Attempt to be smart about when to build CUDA sources for
  -- executables...
  let exesToBuild = map exeName (executables package) `intersect` buildArgs flags
      shouldBuild e = null exesToBuild || exeName e `elem` exesToBuild

  executables' <- forM (filter shouldBuild $ executables package) $ \exe -> do
    let dir = buildDir lbi </> exeName exe
    bi' <- cudaBuild lbi flags dir (buildInfo exe)
    return exe { buildInfo = bi' }

  -- Carry on, given our sneaky modificiations...
  let package' = package { library = library'
                         , executables = executables' }
  buildHook simpleUserHooks package' lbi hooks flags

cudaBuild :: LocalBuildInfo -> BuildFlags -> FilePath -> BuildInfo
          -> IO BuildInfo
cudaBuild lbi flags buildDir bi
  | buildable bi
  , Just cudaSrcLine <- lookup "x-cuda-sources" (customFieldsBI bi)
  = do -- Attempt to parse cuda source line
       let parse rp = map fst . filter (all isSpace . snd) . readP_to_S rp
           parses = parse (parseOptCommaList parseFilePathQ) cudaSrcLine
           cudaSources = head parses
       when (null parses) $ die "Failed to parse x-cuda-sources field."

       -- Get extra cuda command line options
       let cudaOptLine = fromMaybe "" $ lookup "x-cuda-options" (customFieldsBI bi)
           cudaOpts = concat $ parse (sepBy parseTokenQ' (munch1 isSpace)) cudaOptLine

       -- Now compile all sources
       let verbose = buildVerbose flags
           outputFiles = map (flip replaceExtension "o") cudaSources
       (nvcc,_) <- requireProgram verbose nvccProgram (withPrograms lbi)
       forM_ (zip cudaSources outputFiles) $ \(src, out) -> do
           putStrLn $ "Building CUDA source " ++ src ++ "..."
           runProgram verbose nvcc (cudaOpts ++ ["-c", src, "-o", out])

       -- Now for the hacky part: Get the linker to actually link
       -- this. I am 99% sure that this is the wrong way.
       let ldOptions' = ldOptions bi ++ outputFiles

       -- We are not quite done - still need to make sure that the
       -- CUDA libraries can actually get found by the linker. We
       -- simply assume that it sits at a certain path relative to
       -- NVCC - awkward, but that's what the "cuda" package does, so
       -- who am I to judge.
       let cudaBaseDir = takeDirectory $ takeDirectory $ programPath nvcc
           cudaLibDir = cudaBaseDir </> case hostPlatform lbi of
             Platform X86_64 Windows -> "lib" </> "x64"
             Platform I386   Windows -> "lib" </> "Win32"
             Platform X86_64 _       -> "lib64"
             Platform _      _       -> "lib"
           ldOptions'' = ldOptions' ++ ["-L" ++ cudaLibDir]

       return bi { ldOptions = ldOptions'' }
  | otherwise
  = return bi

-- |We slightly change behaviour of cabal pipeline to accomodate for Halide.
main = defaultMainWithHooks dnaProgramsUserHooks
