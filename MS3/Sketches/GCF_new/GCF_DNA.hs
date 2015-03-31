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
import Foreign.Storable.Complex ()
import Foreign.Ptr
import qualified Data.ByteString.Unsafe      as BS
import qualified Data.ByteString             as BS
import qualified Foreign.CUDA.Driver as CUDA

import FFT

import Paths_dna_ms3 ( getDataFileName )

import DNA

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

data GCF = GCF {
    gcfSize   :: !Int
  , gcfNumOfLayers :: !Int
  , gcfPtr    :: !CxDoubleDevPtr
  , gcfLayers :: !(CUDA.DevicePtr CxDoubleDevPtr)
  }

allocateGCF :: Int -> Int -> IO GCF
allocateGCF nOfLayers sizeOfGCF = do
  gcfp <- CUDA.mallocArray sizeOfGCF
  layers <- CUDA.mallocArray nOfLayers
  return $ GCF sizeOfGCF nOfLayers gcfp layers

finalizeGCF :: GCF -> IO ()
finalizeGCF (GCF _ _ gcfp layers) = CUDA.free layers >> CUDA.free gcfp

doCuda :: Double -> [(Double, Int)] -> GCF -> IO ()
doCuda t2 ws_hsupps gcf = do
  m <- CUDA.loadFile =<< getDataFileName "all.cubin"
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
    launchOnFF r2 1 [CUDA.VArg ffp0, CUDA.VArg t2]
    CUDA.allocaArray (256*256) $ \(ffpc :: CxDoubleDevPtr) ->
      CUDA.allocaArray (256*256*8*8) $ \(overo :: CxDoubleDevPtr) ->
        CUDA.allocaArray (256*256*8*8) $ \(overt :: CxDoubleDevPtr) ->
          CUDA.allocaArray 1 $ \(normp :: DoubleDevPtr Double) ->
            let
              go ((w, hsupp):rest) outp0 lptrrlist = do
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
                 let
                   nextPtr = CUDA.advanceDevPtr outp0 $ supp2 * 8 * 8
                 go rest nextPtr (nextPtr:lptrrlist)
              go [] _ lptrrlist = CUDA.pokeListArray (init $ reverse lptrrlist) (gcfLayers gcf)
              gcfptr = gcfPtr gcf
            in go ws_hsupps gcfptr [gcfptr]
  where
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


createGCF :: Double -> [(Double, Int)] -> IO GCF
createGCF t2 ws_hsupps = do
    gcf <- allocateGCF (length ws_hsupps) $ sizeOfGCFInComplexD ws_hsupps
    doCuda t2 ws_hsupps gcf
    return gcf

sizeOfGCFInComplexD :: [(Double, Int)] -> Int
sizeOfGCFInComplexD = sum . map ((*8).(*8).(^(2::Int)).(*2).(+1).snd)

runGCF :: [(Double, Int)] -> IO ()
runGCF ws_hsupps = do
  CUDA.initialise []
  dev0 <- CUDA.device 0
  ctx <- CUDA.create dev0 [CUDA.SchedAuto]
  gcf <- createGCF 0.25 ws_hsupps
  let gsize = gcfSize gcf
  ptr_host <- CUDA.mallocHostArray [] gsize
  CUDA.peekArrayAsync gsize (gcfPtr gcf) ptr_host Nothing
  CUDA.sync
  BS.unsafePackCStringLen (castPtr (CUDA.useHostPtr ptr_host), gsize * sizeOf (undefined :: CxDouble)) >>= BS.writeFile "GCF.dat"
  finalizeGCF gcf
  CUDA.freeHost ptr_host
  CUDA.destroy ctx

main :: IO ()
main = dnaRun id $ flip eval () $ actor $ \() ->
  profile "GCF" [ cudaHint{hintCopyBytesHost = cxDSize * sizeOfGCFInComplexD ws_hsupps} ] $ do
    liftIO $ runGCF ws_hsupps
 where cxDSize = sizeOf (undefined :: CxDouble)
       ws_hsupps = take 31 $ zip [0.0,50.0..] [0,4..]
