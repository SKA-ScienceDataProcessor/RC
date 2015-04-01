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
import qualified Data.ByteString.Unsafe      as BS
import qualified Data.ByteString             as BS
-- import Text.Printf(printf)
import qualified Foreign.CUDA.Driver as CUDA
-- import qualified Foreign.CUDA.Driver.Stream as CUDAS

import DNA

cdSize :: Int
cdSize = sizeOf (undefined :: Complex Double)

runGCF :: ([(Double, Int)], Int) -> IO ()
runGCF ws_hsupps_size = do
  CUDA.initialise []
  dev0 <- CUDA.device 0
  ctx <- CUDA.create dev0 [CUDA.SchedAuto]
  gcf <- createGCF 0.25 ws_hsupps_size
  let gsize = gcfSize gcf
  ptr_host <- CUDA.mallocHostArray [] gsize
  CUDA.peekArrayAsync gsize (gcfPtr gcf) ptr_host Nothing
  CUDA.sync
  BS.unsafePackCStringLen (castPtr (CUDA.useHostPtr ptr_host), gsize * cdSize) >>= BS.writeFile "GCF.dat"
  finalizeGCF gcf
  CUDA.freeHost ptr_host
  CUDA.destroy ctx

main :: IO ()
main = dnaRun id $ flip eval () $ actor $ \() ->
  profile "GCF" [ cudaHint{hintCopyBytesHost = cdSize * snd ws_hsupps_size} ] $ do
    liftIO $ runGCF ws_hsupps_size
 where
   ws_hsupps_size = prepareFullGCF 32 4 50.0
