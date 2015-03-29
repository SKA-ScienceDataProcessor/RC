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
-- import Data.Time.Clock
import System.IO.MMap (
    mmapFilePtr
  , munmapFilePtr
  , Mode(..)
  )
import Text.Printf(printf)
import qualified Foreign.CUDA.Driver as CUDA

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
launchOnFF k xdim  = CUDA.launchKernel k (8,8,xdim) (16,16,1) 0 Nothing

type DoubleDevPtr = CUDA.DevicePtr

launchReduce :: CUDA.Fun -> CxDoubleDevPtr -> DoubleDevPtr Double -> Int -> IO ()
launchReduce f idata odata n =
  CUDA.launchKernel f (n `div` 1024,1,1) (512,1,1) (512 * sizeOf (undefined :: Double)) Nothing
    [CUDA.VArg idata, CUDA.VArg odata, CUDA.IArg $ fromIntegral n]

launchNormalize :: CUDA.Fun -> DoubleDevPtr Double -> CxDoubleDevPtr -> Int32 -> IO ()
launchNormalize f normp ptr len =
  CUDA.launchKernel f (259,1,1) (256,1,1) 0 Nothing
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

doCuda :: [(Double, Int32)] -> IO ()
doCuda ws_hsupps = do
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

  CUDA.allocaArray (257*257) $ \(ffp0 :: CxDoubleDevPtr) -> do
    let t2 = 0.25 :: Double
    print "Preparing r2 ..."
    launchOnFF r2 1 [CUDA.VArg ffp0, CUDA.VArg t2]
    CUDA.allocaArray (257*257) $ \(ffpc :: CxDoubleDevPtr) ->
      CUDA.allocaArray (257*257*8*8) $ \(overo :: CxDoubleDevPtr) ->
        CUDA.allocaArray (257*257*8*8) $ \(overt :: CxDoubleDevPtr) ->
          CUDA.allocaArray 1 $ \(normp :: DoubleDevPtr Double) ->
            let
              go (w, hsupp) = do
                 let
                   supp2 = let supp = 1 + 2 * fromIntegral hsupp in supp * supp
                   suff = if w < 0.0 then "n" else "p"
                   fname = printf "GCF%s%02d.dat" suff hsupp
                 CUDA.sync
                 print fname
                 -- st <- getCurrentTime
                 launchOnFF wkernff 1 [CUDA.VArg ffpc, CUDA.VArg ffp0, CUDA.VArg w]
                 CUDA.memset (CUDA.castDevPtr overo) (257*257*8*8 * 4) (0 :: Int32)
                 CUDA.sync
                 launchOnFF copy_2_over 1 [CUDA.VArg overo, CUDA.VArg ffpc]
                 fft2dComplexDSqInplaceCentered Nothing Inverse (257*8) overo ifftshift_kernel fftshift_kernel
                 launchOnFF transpose_over0 64 [CUDA.VArg overt, CUDA.VArg overo]
                 CUDA.sync
                 let
                   normAndExtractLayers outp layerp n
                     | n > 0 = do
                                 launchReduce reduce_512_odd layerp normp (257*257)
                                 CUDA.sync
                                 launchNormalize normalize normp layerp (257*257)
                                 CUDA.sync
                                 launchOnFF wextract1 1 [CUDA.IArg hsupp, CUDA.VArg outp, CUDA.VArg layerp]
                                 CUDA.sync
                                 normAndExtractLayers (CUDA.advanceDevPtr outp supp2) (CUDA.advanceDevPtr layerp $ 257*257) (n-1)
                     | otherwise = return ()
                 -- We reuse overo array here because we need no it's data anymore
                 --   and next iteration it is zeroed anyway.
                 normAndExtractLayers overo overt (8*8 :: Int)
                 -- ft <- getCurrentTime
                 -- Output the layer
                 (ptr_host, rawsize, offset, _size) <- mmapFilePtr fname ReadWriteEx $ Just (0, supp2 * 16 * 8 * 8)
                 CUDA.peekArray (supp2 * 8 * 8) overo (plusPtr ptr_host offset)
                 munmapFilePtr ptr_host rawsize
                 -- print (diffUTCTime ft st)
            in mapM_ go ws_hsupps
  CUDA.destroy ctx

main :: IO ()
main = doCuda [(-100.0, 8), (0.0, 1), (100.0, 8)]
