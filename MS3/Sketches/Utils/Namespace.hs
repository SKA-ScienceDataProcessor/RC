module Namespace where

import System.Directory
import System.FilePath

createNameSpace :: FilePath -> IO FilePath
createNameSpace fp = do
    onWilkes <- doesDirectoryExist "/ramdisks"
    let fn = if onWilkes then ramdir </> n else n
    createDirectory fn
    return fn
  where
    n = takeBaseName fp
    ramdir = "/ramdisks"

addNameSpace :: FilePath -> FilePath -> IO FilePath
addNameSpace outer inner = createDirectoryIfMissing True d >> return d
  where d = outer </> inner
