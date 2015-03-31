-- | A custom build script for CUDA support.
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
import Distribution.Simple.Utils (die,installOrdinaryFile)

import Distribution.System

import Distribution.PackageDescription hiding (Flag)

import Distribution.Text
import Distribution.ParseUtils
import Distribution.Compat.ReadP

import Distribution.Verbosity

import Data.Maybe
import Data.Char
import Data.List ( intersect )
import Debug.Trace

import System.Process
import System.FilePath

-- | Run cabal, ensuring that CUDA & CUPTI get found
main = defaultMainWithHooks simpleUserHooks {
          -- NB: The following allows us to override NVCC location and
          -- options from the command line. The syntax is slightly
          -- non-obious:
          --
          --  $ cabal configure -- --with-nvcc=[...] --nvcc-options=[...]
          --
          -- Note the extra "--".
          hookedPrograms = nvccProgram : hookedPrograms simpleUserHooks,
          confHook = cudaConfigHook,
          buildHook = cudaBuildHook,
          copyHook = cudaCopyHook
        }

nvccProgram :: Program
nvccProgram = (simpleProgram "nvcc")
              { programFindVersion  = findProgramVersion "--version" verSel }
 where verSel ""     = ""
       verSel output = tail $ last $ words $ last $ lines output

cudaConfigHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags ->
                  IO LocalBuildInfo
cudaConfigHook dat flags = do

  let verbose = fromFlagOrDefault normal $ configVerbosity flags
  m_nvccPath <- findProgramOnSearchPath verbose [ProgramSearchPathDefault] "nvcc"

  -- Irrespectively of whether there are actual CUDA sources or not,
  -- we want to make sure that CUDA & CUPTI libraries can actually get
  -- found by the linker. We simply assume that it sits at a certain
  -- path relative to NVCC - awkward, but that's what the "cuda"
  -- package does, so who am I to judge.
  let flags' nvcc = flags { configExtraLibDirs = configExtraLibDirs flags ++
                                                 [cudaLibDir, cuptiLibDir]
                          , configExtraIncludeDirs = configExtraIncludeDirs flags ++
                                                     [cudaIncDir, cuptiIncDir]
                          }
        where cudaBaseDir = takeDirectory $ takeDirectory nvcc
              cudaLibDir = cudaBaseDir </> libDir
              cudaIncDir = cudaBaseDir </> "include"
              cuptiBaseDir = cudaBaseDir </> "extras" </> "CUPTI"
              cuptiLibDir = cuptiBaseDir </> libDir
              cuptiIncDir = cuptiBaseDir </> "include"
              libDir = case buildPlatform of
                         Platform X86_64 Windows -> "lib" </> "x64"
                         Platform I386   Windows -> "lib" </> "Win32"
                         Platform X86_64 _       -> "lib64"
                         Platform _      _       -> "lib"

  case m_nvccPath of
    Just nvcc -> putStrLn ("Found CUDA in " ++ takeDirectory (takeDirectory nvcc)) >>
                 confHook simpleUserHooks dat (flags' nvcc)
    Nothing   -> confHook simpleUserHooks dat flags

-- | Finds CUDA sources to build from a package description. The
-- returned tuple is executable name, CUDA source, then extra command
-- line options.
findCuda :: PackageDescription -> [(Maybe String, FilePath, [String])]
findCuda package
  = (maybe [] (findCudaBI Nothing . libBuildInfo) (library package)) ++
    concatMap findCudaExe (executables package)
  where findCudaExe exe
            | buildable (buildInfo exe) = findCudaBI (Just $ exeName exe) (buildInfo exe)
            | otherwise                 = []
        findCudaBI name bi
            | Just cudaSrcLine <- lookup "x-cuda-sources" (customFieldsBI bi)
            , let parse rp = map fst . filter (all isSpace . snd) . readP_to_S rp
                  parses = parse (parseOptCommaList parseFilePathQ) cudaSrcLine
                  cudaSources = head parses
                  cudaOptLine = fromMaybe "" $ lookup "x-cuda-options" (customFieldsBI bi)
                  cudaOpts = concat $ parse (sepBy parseTokenQ' (munch1 isSpace)) cudaOptLine
            = [ (name, cudaFile, cudaOpts) | cudaFile <- cudaSources ]
            | otherwise
            = []

buildCuda :: Bool -> PackageDescription -> LocalBuildInfo -> Flag Verbosity -> IO [FilePath]
buildCuda doBuild package lbi verbosity = do

  -- Find CUDA sources
  let cudaSources = findCuda package
  if null cudaSources then return [] else do

    -- Attempt to be smart about when to build CUDA sources for
    -- executables...
    let exesToBuild = map exeName (executables package) -- `intersect` buildArgs flags
        shouldBuild Nothing  = True -- always build library
        shouldBuild (Just e) = null exesToBuild || exeName e `elem` exesToBuild

    -- Now compile all sources
    let verbose = fromFlagOrDefault normal verbosity
    (nvcc,_) <- requireProgram verbose nvccProgram (withPrograms lbi)
    forM cudaSources $ \(name, cudaFile, cudaOpts) -> do
        let out = maybe (buildDir lbi) (buildDir lbi </>) name
                  </> replaceExtension (takeFileName cudaFile) "cubin"
        when doBuild $ do
             putStrLn $ "Building CUDA source " ++ cudaFile ++ "..."
             runProgram verbose nvcc (cudaOpts ++ ["--cubin", cudaFile, "-o", out])
        return out

cudaBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
cudaBuildHook package lbi hooks flags = do
  void $ buildCuda True package lbi (buildVerbosity flags)
  buildHook simpleUserHooks package lbi hooks flags

cudaCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
cudaCopyHook package lbi hooks flags = do
  outs <- buildCuda False package lbi (copyVerbosity flags)
  let installDirs = absoluteInstallDirs package lbi (fromFlag (copyDest flags))
  forM_ outs $ \file ->
      installOrdinaryFile (fromFlag $ copyVerbosity flags) file
          (datadir installDirs </> takeFileName file)
  copyHook simpleUserHooks package lbi hooks flags
