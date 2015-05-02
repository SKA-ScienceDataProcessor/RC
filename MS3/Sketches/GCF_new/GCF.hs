{-# LANGUAGE
      CPP
    , ScopedTypeVariables
    , FlexibleInstances
    , TypeSynonymInstances
    , DeriveGeneric
    , DeriveDataTypeable
    , BangPatterns
  #-}

module GCF (
    prepareGCF
  , createGCF
  , finalizeGCF
  , finalizeGCFHost
  , marshalGCF2Host
  , getLayers
  , getLayersHost
  , GCF(..)
  , GCFDev
  , GCFHost
  , GCFCfg(..)
  , writeGCFHostToFile
  , readGCFHostFromFile
  ) where

import Control.Monad

import Foreign.Storable
import Foreign.Storable.Complex ()
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import qualified CUDAEx as CUDA
import CUDAEx (CxDoubleDevPtr, CxDouble)

import GHC.Generics (Generic)
import GHC.Exts (Ptr(..))
import Data.Binary
import BinaryInstances ()
import Data.Typeable

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as IBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as UBS
import Data.Binary.Put
import Data.Binary.Get

import ArgMapper
import FFT

import System.IO

launchOnFF :: CUDA.Fun -> Int -> [CUDA.FunParam] -> IO ()
launchOnFF k xdim  = CUDA.launchKernel k (8,8,xdim) (32,32,1) 0 Nothing

type DoubleDevPtr = CUDA.DevicePtr Double

launchReduce :: CUDA.Fun -> CxDoubleDevPtr -> DoubleDevPtr -> Int -> IO ()
launchReduce f idata odata n =
  CUDA.launchKernel f (n `div` 1024,1,1) (512,1,1) (fromIntegral $ 512 * sizeOf (undefined :: Double)) Nothing
    |< idata :. odata :. n :. Z

launchNormalize :: CUDA.Fun -> DoubleDevPtr -> CxDoubleDevPtr -> Int -> IO ()
launchNormalize f normp ptr len =
  CUDA.launchKernel f (128,1,1) (512,1,1) 0 Nothing
    |< normp :. ptr :. len :. Z

data GCF ptrt = GCF {
    gcfSize   :: !Int
  , gcfNumOfLayers :: !Int
  , gcfPtr    :: !(ptrt CxDouble)
  , gcfLayers :: !(ptrt (ptrt CxDouble))
  , isFull :: !Bool
  } deriving (Generic, Typeable)

type GCFDev = GCF CUDA.DevicePtr
type GCFHost = GCF Ptr

instance Binary GCFDev
instance Binary GCFHost

getLayers :: GCFDev -> CUDA.DevicePtr CxDoubleDevPtr
getLayers (GCF _ n _ l isfull) = if isfull then CUDA.advanceDevPtr l ((n `div` 2) * 8 * 8) else l

getLayersHost :: GCFHost -> Ptr (Ptr CxDouble)
getLayersHost (GCF _ n _ l isfull) = if isfull then advancePtr l ((n `div` 2) * 8 * 8) else l

allocateGCF :: Bool -> Int -> Int -> IO GCFDev
allocateGCF isfull nOfLayers sizeOfGCFInComplexD = do
  gcfp <- CUDA.mallocArray sizeOfGCFInComplexD
  layers <- CUDA.mallocArray (nOfLayers * 8 * 8)
  return $ GCF sizeOfGCFInComplexD nOfLayers gcfp layers isfull

allocateGCFHost :: Bool -> Int -> Int -> IO GCFHost
allocateGCFHost isfull nOfLayers sizeOfGCFInComplexD = do
  gcfp <- CUDA.mallocHostArray [] sizeOfGCFInComplexD
  layers <- CUDA.mallocHostArray [] (nOfLayers * 8 * 8)
  return $ GCF sizeOfGCFInComplexD nOfLayers (CUDA.useHostPtr gcfp) (CUDA.useHostPtr layers) isfull

marshalGCF2Host :: GCFDev -> IO GCFHost
marshalGCF2Host (GCF size nol gcfp lrsp isfull) = do
    gcfh@(GCF _ _ gcfhp lrshp _) <- allocateGCFHost isfull nol size
    CUDA.peekArrayAsync size gcfp (CUDA.HostPtr gcfhp) Nothing
    CUDA.peekArrayAsync lsiz lrsp (CUDA.HostPtr $ castPtr lrshp) Nothing
    CUDA.sync
    let
      lptrend = advancePtr lrshp lsiz
      gcf_p = CUDA.useDevicePtr gcfp
      go lptr
        | lptr < lptrend = do
            dlptr <- peek lptr
            let off = minusPtr dlptr gcf_p
            poke lptr (plusPtr gcfhp off)
            go (advancePtr lptr 1)
        | otherwise = return ()
    go lrshp
    return gcfh
  where
    lsiz = nol * 8 * 8

finalizeGCF :: GCFDev -> IO ()
finalizeGCF (GCF _ _ gcfp layers _) = CUDA.free layers >> CUDA.free gcfp

finalizeGCFHost :: GCFHost -> IO ()
finalizeGCFHost (GCF _ _ gcfp layers _) =
  CUDA.freeHost (CUDA.HostPtr layers) >> CUDA.freeHost (CUDA.HostPtr gcfp)

writeGCFHostToFile :: GCFHost -> FilePath -> IO ()
writeGCFHostToFile (GCF size nol gcfp lrsp _) file = do
  fd <- openFile file WriteMode
  LBS.hPut fd $ runPut $ put size >> put nol
  let lsiz = nol * 8 * 8
  forM_ [0..lsiz-1] $ \i -> do
    v <- peekElemOff lrsp i
    LBS.hPut fd $ runPut $ put $ v `minusPtr` gcfp
  let !(Ptr addr) = gcfp
      cdSize = sizeOf (undefined :: CxDouble)
  BS.hPut fd =<< UBS.unsafePackAddressLen (size * cdSize) addr
  hClose fd

readGCFHostFromFile :: Bool -> FilePath -> IO GCFHost
readGCFHostFromFile isfull file = do
  fd <- openFile file ReadMode
  let intSize = sizeOf (undefined :: Int)
  cts <- LBS.hGet fd (intSize * 2)
  let (size, nol) = flip runGet cts $ liftM2 (,) get get
  let lsiz = nol * 8 * 8
  gcf@(GCF _ _ gcfp lrsp _) <- allocateGCFHost isfull nol size
  forM_ [0..lsiz-1] $ \i -> do
    v <- runGet get `liftM` LBS.hGet fd intSize
    pokeElemOff lrsp i (gcfp `plusPtr` v)
  let cdSize = sizeOf (undefined :: CxDouble)
  bs <- BS.hGet fd (size * cdSize)
  let (fp, off, l) = IBS.toForeignPtr bs
  withForeignPtr fp $ \p -> copyBytes gcfp (p `plusPtr` off) l
  return gcf

#define __k(n) foreign import ccall unsafe "&" n :: CUDA.Fun

__k(ifftshift_kernel)
__k(reduce_512_e2)
__k(r2)
__k(wkernff)
__k(copy_2_over)
__k(transpose_over0)
__k(normalize)
__k(wextract1)

doCuda :: Double -> [(Double, Int)] -> GCFDev -> IO ()
doCuda t2 ws_hsupps gcf =
  CUDA.allocaArray (256*256) $ \(ffp0 :: CxDoubleDevPtr) -> do
    launchOnFF r2 1 |< ffp0 :. t2 :. Z
    CUDA.allocaArray (256*256) $ \(ffpc :: CxDoubleDevPtr) ->
      CUDA.allocaArray (256*256*8*8) $ \(overo :: CxDoubleDevPtr) ->
        CUDA.allocaArray (256*256*8*8) $ \(overt :: CxDoubleDevPtr) ->
          CUDA.allocaArray 1 $ \(normp :: DoubleDevPtr) ->
            let
              go ((w, hsupp):rest) lptrrlist = do
                 let
                   supp2 = let supp = 1 + 2 * fromIntegral hsupp in supp * supp
                 CUDA.sync
                 launchOnFF wkernff 1 |< ffpc :. ffp0 :. w :. Z
                 CUDA.memset (CUDA.castDevPtr overo) (256*256*8*8 * 16) 0
                 CUDA.sync
                 launchOnFF copy_2_over 1 |< overo :. ffpc :. Z
                 fft2dComplexDSqInplaceCentered Nothing Inverse (256*8) overo ifftshift_kernel fftshift_kernel
                 launchOnFF transpose_over0 64 |< overt :. overo :. Z
                 CUDA.sync
                 let
                   normAndExtractLayers outplist layerp n
                     | n > 0 = do
                                 launchReduce reduce_512_e2 layerp normp (256*256)
                                 CUDA.sync
                                 -- CUDA.peekArray 1 normp hnormp -- DEBUG
                                 -- peek hnormp >>= print
                                 launchNormalize normalize normp layerp (256*256)
                                 CUDA.sync
                                 let outp = head outplist
                                 launchOnFF wextract1 1 |< hsupp :. outp :. layerp :. Z
                                 CUDA.sync
                                 let nextPtr = CUDA.advanceDevPtr outp supp2
                                 normAndExtractLayers (nextPtr:outplist) (CUDA.advanceDevPtr layerp $ 256*256) (n-1)
                     | otherwise = return outplist
                 olist <- normAndExtractLayers lptrrlist overt (8*8 :: Int)
                 go rest olist
              go [] lptrrlist = CUDA.pokeListArray (init $ reverse lptrrlist) (gcfLayers gcf)
            in go ws_hsupps [gcfPtr gcf]

type LD = [(Double, Int)]

data GCFCfg = GCFCfg {
    gcfcSuppDiv2Step :: Int
  , gcfcLayersDiv2Plus1 :: Int
  , gcfcIsFull :: Bool
  , gcfcT2     :: Double
  , gcfcWStep  :: Double
} deriving (Generic, Typeable)

instance Binary GCFCfg

prepareGCF :: Bool -> Int -> Int -> Int -> Double -> (LD, Int)
prepareGCF isfull n supp0 hsupp_step wstep = (wsp, sizeOfGCFInComplexD)
  where
    wsp0 = take n $ iterate (\(w, hs) -> (w + wstep, hs + hsupp_step)) (0.0, 0)
    wsp = if isfull
            then map (\(w,h) -> (-w,h)) (reverse $ tail wsp0) ++ wsp0
            else wsp0
    sizeOfGCFInComplexD = (sum $ map (\(_, hsupp) -> let supp = 2 * hsupp + supp0 in supp * supp) wsp) * 8 * 8

createGCF :: Bool -> Double -> (LD, Int) -> IO GCFDev
createGCF isfull t2 (wsp, sizeOfGCFInComplexD) = do
    CUDA.sync
    gcf <- allocateGCF isfull (length wsp) sizeOfGCFInComplexD
    doCuda t2 wsp gcf
    return gcf {isFull = isfull}
