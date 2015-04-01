{-# LANGUAGE
      CPP
    , ScopedTypeVariables
    , FlexibleInstances
    , TypeSynonymInstances
    , DeriveGeneric
  #-}

module GCF (
    prepareHalfGCF
  , prepareFullGCF
  , createGCF
  , finalizeGCF
  , getCentreOfFullGCF
  , GCF(..)
  ) where

import Data.Int
import Foreign.Storable
import Foreign.Storable.Complex ()
-- import Foreign.Marshal.Alloc -- for DEBUG only!
-- import Text.Printf(printf)
import qualified Foreign.CUDA.Driver as CUDA
-- import qualified Foreign.CUDA.Driver.Stream as CUDAS

import GHC.Generics (Generic)
import Data.Binary
import BinaryInstances ()

import FFT

#ifndef QUICK_TEST
import Paths_dna_ms3 ( getDataFileName )
#else
#define getDataFileName return
#endif

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
  } deriving (Generic)

instance Binary GCF

getCentreOfFullGCF :: GCF -> CUDA.DevicePtr CxDoubleDevPtr
getCentreOfFullGCF (GCF _ n _ l) = CUDA.advanceDevPtr l (n `div` 2)

allocateGCF :: Int -> Int -> IO GCF
allocateGCF nOfLayers sizeOfGCFInComplexD = do
  gcfp <- CUDA.mallocArray sizeOfGCFInComplexD
  layers <- CUDA.mallocArray nOfLayers
  return $ GCF sizeOfGCFInComplexD nOfLayers gcfp layers

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


type LD = [(Double, Int)]

prepareGCFWith :: (LD -> LD) -> Int -> Int -> Double -> (LD, Int)
prepareGCFWith mirror n hsupp_step wstep = (wsp, sizeOfGCFInComplexD)
  where
    wsp0 = take n $ iterate (\(w, hs) -> (w + wstep, hs + hsupp_step)) (0.0, 0)
    wsp = mirror wsp0 ++ wsp0
    sizeOfGCFInComplexD = (sum $ map (\(_, hsupp) -> let supp = 2 * hsupp + 1 in supp * supp) wsp) * 8 * 8

prepareHalfGCF, prepareFullGCF :: Int -> Int -> Double -> (LD, Int)
prepareHalfGCF = prepareGCFWith (const [])
prepareFullGCF = prepareGCFWith mirror
  where
    mirror wsp0 = map (\(w,h) -> (-w,h)) (reverse $ tail wsp0)

createGCF :: Double -> (LD, Int) -> IO GCF
createGCF t2 (wsp, sizeOfGCFInComplexD) = do
    gcf <- allocateGCF (length wsp) sizeOfGCFInComplexD
    doCuda t2 wsp gcf
    return gcf
