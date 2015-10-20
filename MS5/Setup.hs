-- | A custom build script for CUDA support.
--
-- Copyright (C) 2014 Braam Research, LLC.

module Main(main) where

import Control.Applicative
import Control.Monad

import Distribution.Simple
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.Configure (configure)
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Find
import Distribution.Simple.Program.Types
import Distribution.Simple.Utils

import Distribution.System

import Distribution.PackageDescription as PD hiding (Flag)

import Distribution.Text
import Distribution.ParseUtils
import Distribution.Compat.ReadP

import Distribution.Verbosity

import Data.Maybe
import Data.Char
import Data.List ( intersect )
import Debug.Trace

import System.Directory
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
                                                 [cudaLibDir]
                          , configExtraIncludeDirs = configExtraIncludeDirs flags ++
                                                     [cudaIncDir]
                          }
        where cudaBaseDir = takeDirectory $ takeDirectory nvcc
              cudaLibDir = cudaBaseDir </> libDir
              cudaIncDir = cudaBaseDir </> "include"
              libDir = case buildPlatform of
                         Platform X86_64 Windows -> "lib" </> "x64"
                         Platform I386   Windows -> "lib" </> "Win32"
                         Platform X86_64 _       -> "lib64"
                         Platform _      _       -> "lib"

  case m_nvccPath of
    Just nvcc -> putStrLn ("Found CUDA in " ++ takeDirectory (takeDirectory nvcc)) >>
                 confHook simpleUserHooks dat (flags' nvcc)
    Nothing   -> confHook simpleUserHooks dat flags

buildCuda :: Bool -> PackageDescription -> LocalBuildInfo -> Verbosity
          -> IO (PackageDescription, [FilePath])
buildCuda doBuild package lbi verbose = do

  -- Find all CUDA sources in libraries & executables. Update
  -- build information accordingly.
  (library', lib_cubins) <- case library package of
    Just lib -> do (bi', cubins) <- cudaBuildInfo doBuild lbi verbose
                                                  (buildDir lbi) "" (libBuildInfo lib)
                   return (Just lib { libBuildInfo = bi' }, cubins)
    Nothing  -> return (Nothing, [])

  -- Attempt to be smart about when to build CUDA sources for
  -- executables...
  let exesToBuild = map exeName (executables package) -- `intersect` buildArgs flags
      shouldBuild e = buildable (buildInfo e) &&
                      (null exesToBuild || exeName e `elem` exesToBuild)

  exe_cubinss <- forM (filter shouldBuild $ executables package) $ \exe -> do

    -- Build directory & real exe name, copied from Distribution/Simple/GHC.hs.
    -- Would be brilliant if there was a more direct way to get this...
    let dir = buildDir lbi </> exeName exe
        exeNameReal = exeName exe <.> (if takeExtension (exeName exe) /= ('.':exeExtension)
                                       then exeExtension
                                       else "")

    (bi', cubins) <- cudaBuildInfo doBuild lbi verbose dir exeNameReal (buildInfo exe)
    return (exe { buildInfo = bi' }, cubins)
  let (executables', cubinss) = unzip exe_cubinss

  -- Carry on, given our sneaky modificiations...
  return (package { library = library'
                  , executables = executables' },
          lib_cubins ++ concat cubinss)

cudaBuildInfo :: Bool -> LocalBuildInfo -> Verbosity -> FilePath -> FilePath -> BuildInfo
              -> IO (BuildInfo, [FilePath])
cudaBuildInfo doBuild lbi verbose buildDir nameReal bi = do

  -- Get CUDA command line options
  let parseOpt rp = map fst . filter (all isSpace . snd) . readP_to_S rp
      cudaOptLine = fromMaybe "" $ lookup "x-cuda-options" (customFieldsBI bi)
      cudaOpts = concat $ parseOpt (sepBy parseTokenQ' (munch1 isSpace)) cudaOptLine

  -- Prepare for building
  (nvcc,_) <- requireProgram verbose nvccProgram (withPrograms lbi)
  (gcc,_) <- requireProgram verbose gccProgram (withPrograms lbi)
  let mkOutput ext = (buildDir </>) . flip replaceExtension ext . takeFileName
  when doBuild $ createDirectoryIfMissingVerbose verbose True buildDir

  -- Rebuild check
  let checkRebuild src out io = do
        srcMoreRecent <- moreRecentFile src out
        cabalMoreRecent <- maybe (return False) (flip moreRecentFile out)  (pkgDescrFile lbi)
        when (srcMoreRecent || cabalMoreRecent) io

  -- Force rebuilding the library/executable by deleting it
  let invalidate = removeFile $ buildDir </> nameReal

  -- Build CUBINs
  cubins <- case lookup "x-cuda-sources-cubin" (customFieldsBI bi) of
    Nothing          -> return []
    Just cudaSrcLine -> do
       let parses = parseOpt (parseOptCommaList parseFilePathQ) cudaSrcLine
           cudaSources = head parses
       when (null parses) $ die "Failed to parse x-cuda-sources-cubin field."

       let outputFiles = map (mkOutput "cubin") cudaSources
       when doBuild $ forM_ (zip cudaSources outputFiles) $ \(src, out) ->
           checkRebuild src out $ do
               putStrLn $ "Building CUDA source " ++ src ++ "..."
               invalidate
               runProgram verbose nvcc (cudaOpts ++ ["--cubin", src, "-o", out])
       return outputFiles

  -- Build CUDA object files
  bi' <- case lookup "x-cuda-sources" (customFieldsBI bi) of
     Nothing          -> return bi
     Just cudaSrcLine -> do
       let parses = parseOpt (parseOptCommaList parseFilePathQ) cudaSrcLine
           cudaSources = head parses
       when (null parses) $ die "Failed to parse x-cuda-sources field."

       let outputFiles = map (mkOutput "o") cudaSources
       when doBuild $ forM_ (zip cudaSources outputFiles) $ \(src, out) -> 
           checkRebuild src out $ do
               putStrLn $ "Building CUDA source " ++ src ++ "..."
               invalidate
               runProgram verbose nvcc (cudaOpts ++ ["-c", src, "-o", out])

       -- Now for the hacky part: Get the linker to actually link
       -- this. I am 99% sure that this is the wrong way. In fact, it
       -- will fail to pick up the object file for ".a" libraries.
       return  bi { ldOptions = ldOptions bi ++ outputFiles }

  -- Finally build Halide object files
  let halideOptLine = fromMaybe "" $ lookup "x-halide-options" (customFieldsBI bi)
      halideOpts = concat $ parseOpt (sepBy parseTokenQ' (munch1 isSpace)) halideOptLine
  bi'' <- case lookup "x-halide-sources" (customFieldsBI bi) of
     Nothing          -> return bi
     Just cudaSrcLine -> do

       let parses = parseOpt (parseOptCommaList parseFilePathQ) cudaSrcLine
           halideSources = head parses
       when (null parses) $ die "Failed to parse x-halide-sources field."

       let genFiles = map (mkOutput "gen") halideSources
           outputFiles = map (mkOutput "kern.o") halideSources
       when doBuild $ forM_ (zip3 halideSources genFiles outputFiles) $ \(src, gen, out) ->
           checkRebuild src out $ do
               putStrLn $ "Building Halide source " ++ src ++ "..."
               invalidate
               runProgram verbose gcc $ concat
                 [ halideOpts
                 , map ("-I"++) (PD.includeDirs bi)
                 , map ("-L"++) (PD.extraLibDirs bi)
                 , [src, "-o", gen]]
               runProgramInvocation verbose $ simpleProgramInvocation gen [out]

       -- Yet again, hackily link the results in.
       return bi { ldOptions = ldOptions bi ++ outputFiles }

  return (bi'', cubins)

cudaBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
cudaBuildHook package lbi hooks flags = do
  (package', _) <- buildCuda True package lbi (fromFlag $ buildVerbosity flags)
  buildHook simpleUserHooks package' lbi hooks flags

cudaCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
cudaCopyHook package lbi hooks flags = do
  let verbose = fromFlag $ copyVerbosity flags
  (package', outs) <- buildCuda False package lbi verbose
  let installDirs = absoluteInstallDirs package lbi (fromFlag (copyDest flags))
  createDirectoryIfMissingVerbose verbose True (datadir installDirs)
  forM_ outs $ \file ->
      installOrdinaryFile (fromFlag $ copyVerbosity flags) file
          (datadir installDirs </> takeFileName file)
  copyHook simpleUserHooks package' lbi hooks flags
