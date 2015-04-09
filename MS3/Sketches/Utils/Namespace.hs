module Namespace where

import System.Directory
import System.FilePath

createNameSpace :: FilePath -> IO ()
createNameSpace fp = do
    onWilkes <- doesDirectoryExist "/ramdisks"
    let fn = if onWilkes then ramdir </> n else n
    createDirectory fn
  where
    n = takeBaseName fp
    ramdir = "/ramdisks"

addNameSpace :: FilePath -> FilePath -> IO ()
addNameSpace outer inner = createDirectoryIfMissing True $ outer </> inner
