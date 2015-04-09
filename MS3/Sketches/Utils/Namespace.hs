module Namespace where

import System.Directory
import System.FilePath

data NSType = RAM | Persistent

isRam :: NSType -> Bool
isRam RAM = True
isRam _   = False

createNameSpace :: NSType -> FilePath -> IO FilePath
createNameSpace nstyp fp = do
    onWilkes <- doesDirectoryExist ramdir
    let fn = if onWilkes && (isRam nstyp) then ramdir </> n else n
    createDirectory fn
    return fn
  where
    n = takeBaseName fp
    ramdir = "/ramdisks"

addNameSpace :: FilePath -> FilePath -> IO FilePath
addNameSpace outer inner = createDirectoryIfMissing True d >> return d
  where d = outer </> inner
