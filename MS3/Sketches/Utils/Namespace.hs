module Namespace where

import Control.Monad ( liftM, filterM )

import System.Directory
import System.FilePath

data NSType = RAM | Persistent

isRam :: NSType -> Bool
isRam RAM = True
isRam _   = False

createNameSpace :: NSType -> FilePath -> IO FilePath
createNameSpace nstyp fp = do
    let dirs = case nstyp of
          RAM        -> ["/ramdisks", "/tmp", "."]
          Persistent -> ["."]
    dir <- head `liftM` filterM doesDirectoryExist dirs
    let fn = dir </> takeBaseName fp
    createDirectoryIfMissing True fn
    return fn

addNameSpace :: FilePath -> FilePath -> IO FilePath
addNameSpace outer inner = createDirectoryIfMissing True d >> return d
  where d = outer </> inner
