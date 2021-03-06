{-# LANGUAGE
      CPP
    , ScopedTypeVariables
    , FlexibleInstances
    , TypeSynonymInstances
  #-}

module Main where

import GCF

import Data.Complex
import Foreign.Storable.Complex ()
import Foreign
import Data.Time.Clock
import qualified Data.ByteString.Unsafe      as BS
import qualified Data.ByteString             as BS
-- import Text.Printf(printf)
import qualified CUDAEx as CUDA

main :: IO ()
main = do
  CUDA.get >>= print

  t0 <- getCurrentTime
  gcf <- createGCF 0.25 $ prepareFullGCF 11 4 50.0
  t1 <- getCurrentTime

  let gsize = gcfSize gcf
  ptr_host <- CUDA.mallocHostArray [] gsize
  CUDA.peekArrayAsync gsize (gcfPtr gcf) ptr_host Nothing
  CUDA.sync
  BS.unsafePackCStringLen (castPtr (CUDA.useHostPtr ptr_host), gsize * sizeOf (undefined :: Complex Double)) >>= BS.writeFile "GCF.dat"
  t2 <- getCurrentTime
  finalizeGCF gcf
  CUDA.freeHost ptr_host
  CUDA.reset

  print (diffUTCTime t1 t0)
  print (diffUTCTime t2 t1)
