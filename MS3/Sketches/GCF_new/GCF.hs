{-# LANGUAGE
      CPP
    , ScopedTypeVariables
    , FlexibleInstances
    , TypeSynonymInstances
  #-}

module Main where

import Data.Complex
import Data.Int
import Foreign.Storable
import Foreign.Ptr
-- import Foreign.Marshal.Alloc -- for DEBUG only!
import Data.Time.Clock
{-
import System.IO.MMap (
    mmapFilePtr
  , munmapFilePtr
  , Mode(..)
  )
 -}
import qualified Data.ByteString.Unsafe      as BS
import qualified Data.ByteString             as BS
-- import Text.Printf(printf)
import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Stream as CUDAS

import FFT

-- Quick and dirty storable for Complex
instance Storable CxDouble where
  sizeOf _ = 16
  alignment _ = 16
  peek p = do
    re <- peek (castPtr p)
    im <- peekByteOff p 8
    return (re :+ im)
  poke p (re :+ im) = do
    poke (castPtr p) re
    pokeByteOff p 8 im

launchOnFF :: CUDA.Fun -> Int -> [CUDA.FunParam] -> IO ()
launchOnFF k xdim  = CUDA.launchKernel k (8,8,xdim) (32,32,1) 0 Nothing

type DoubleDevPtr = CUDA.DevicePtr

launchReduce :: CUDA.Fun -> CxDoubleDevPtr -> DoubleDevPtr Double -> Int -> IO ()
launchReduce f idata odata n =
  CUDA.launchKernel f (n `div` 1024,1,1) (512,1,1) (512 * sizeOf (undefined :: Double)) Nothing
    [CUDA.VArg idata, CUDA.VArg odata, CUDA.IArg $ fromIntegral n]

launchNormalize :: CUDA.Fun -> DoubleDevPtr Double -> CxDoubleDevPtr -> Int32 -> IO ()
launchNormalize f normp ptr len =
  CUDA.launchKernel f (128,1,1) (512,1,1) 0 Nothing
    [CUDA.VArg normp, CUDA.VArg ptr, CUDA.IArg len]

kernelNames :: [String]
kernelNames =
  [ "fftshift_kernel"
  , "ifftshift_kernel"
  , "reduce_512_odd"
  , "r2"
  , "wkernff"
  , "copy_2_over"
  , "transpose_over0"
  , "normalize"
  , "wextract1"
  ]

doCuda :: Double -> [(Double, Int)] -> Int -> IO ()
doCuda t2 ws_hsupps gcfSize = do
  -- hnormp <- malloc  -- for DEBUG only!
  CUDA.initialise []
  dev0 <- CUDA.device 0
  ctx <- CUDA.create dev0 [CUDA.SchedAuto]
  m <- CUDA.loadFile "all.cubin"
  [  fftshift_kernel
   , ifftshift_kernel
   , reduce_512_odd
   , r2
   , wkernff
   , copy_2_over
   , transpose_over0
   , normalize
   , wextract1 ] <- mapM (CUDA.getFun m) kernelNames

  CUDA.allocaArray (256*256) $ \(ffp0 :: CxDoubleDevPtr) -> do
    print "Preparing r2 ..."
    launchOnFF r2 1 [CUDA.VArg ffp0, CUDA.VArg t2]
    CUDA.allocaArray (256*256) $ \(ffpc :: CxDoubleDevPtr) ->
      CUDA.allocaArray (256*256*8*8) $ \(overo :: CxDoubleDevPtr) ->
        CUDA.allocaArray (256*256*8*8) $ \(overt :: CxDoubleDevPtr) ->
          -- FIXME: Factor CUDA initialization and host ptr creation out
          CUDA.allocaArray gcfSize $ \(out :: CxDoubleDevPtr) ->
            CUDA.allocaArray 1 $ \(normp :: DoubleDevPtr Double) -> do
              st <- getCurrentTime
              let
                go ((w, hsupp):rest) outp0 = do
                   let
                     supp2 = let supp = 1 + 2 * fromIntegral hsupp in supp * supp
                   CUDA.sync
                   launchOnFF wkernff 1 [CUDA.VArg ffpc, CUDA.VArg ffp0, CUDA.VArg w]
                   CUDA.memset (CUDA.castDevPtr overo) (256*256*8*8 * 4) (0 :: Int32)
                   CUDA.sync
                   launchOnFF copy_2_over 1 [CUDA.VArg overo, CUDA.VArg ffpc]
                   fft2dComplexDSqInplaceCentered Nothing Inverse (256*8) overo ifftshift_kernel fftshift_kernel
                   launchOnFF transpose_over0 64 [CUDA.VArg overt, CUDA.VArg overo]
                   CUDA.sync
                   let
                     normAndExtractLayers outp layerp n
                       | n > 0 = do
                                   launchReduce reduce_512_odd layerp normp (256*256)
                                   CUDA.sync
                                   -- CUDA.peekArray 1 normp hnormp -- DEBUG
                                   -- peek hnormp >>= print
                                   launchNormalize normalize normp layerp (256*256)
                                   CUDA.sync
                                   launchOnFF wextract1 1 [CUDA.IArg (fromIntegral hsupp), CUDA.VArg outp, CUDA.VArg layerp]
                                   CUDA.sync
                                   normAndExtractLayers (CUDA.advanceDevPtr outp supp2) (CUDA.advanceDevPtr layerp $ 256*256) (n-1)
                       | otherwise = return ()
                   normAndExtractLayers outp0 overt (8*8 :: Int)
                   go rest (CUDA.advanceDevPtr outp0 $ supp2 * 8 * 8)
                go [] _ = do
                            ft <- getCurrentTime
                            print (diffUTCTime ft st)
                            {-
                            (ptr_host, rawsize, offset, _size) <- mmapFilePtr "GCF.dat" ReadWriteEx $ Just (0, gcfSize * 16)
                            CUDA.peekArray gcfSize out (plusPtr ptr_host offset)
                            munmapFilePtr ptr_host rawsize
                            -}
                            ptr_host <- CUDA.mallocHostArray [CUDA.DeviceMapped, CUDA.WriteCombined] gcfSize
                            writeStreams <- sequence $ replicate 16 (CUDAS.create [])
                            let chunkSize = gcfSize `div` 16
                            mapM_ (\(i, str) -> 
                                      let off = chunkSize * i
                                      in CUDA.peekArrayAsync chunkSize (CUDA.advanceDevPtr out off) (CUDA.advanceHostPtr ptr_host off) (Just str)
                              ) $ zip [0..15] writeStreams
                            CUDA.sync
                            mapM_ (CUDAS.destroy) writeStreams
                            BS.unsafePackCStringLen (castPtr (CUDA.useHostPtr ptr_host), gcfSize * sizeOf (undefined :: CxDouble)) >>= BS.writeFile "GCF.dat"
                            CUDA.freeHost ptr_host
                            ft1 <- getCurrentTime
                            print (diffUTCTime ft1 ft)
              go ws_hsupps out
  CUDA.destroy ctx

doit :: Int -> Double -> Int -> Double -> IO ()
doit n t2 hsupp_step wstep = doCuda t2 wsp sizeOfGCFInComplexD
  where
    wsp = take n $ iterate (\(w, hs) -> (w + wstep, hs + hsupp_step)) (0.0, 0)
    sizeOfGCFInComplexD = sum $ map (\(_, hsupp) -> let supp = 2 * hsupp + 1 in supp * supp * 8 * 8) wsp

main :: IO ()
main = doit 32 0.25 4 50.0
