module Kernel.GPU.GatherGrid
  ( prepare
  , createGrid
  , grid
  , degrid
  ) where

import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array ( advancePtr, withArray )
import Foreign.Storable ( peek, sizeOf )
import Kernel.GPU.Common

import Data
import Vector

import qualified Kernel.GPU.NvidiaDegrid as Nvidia

foreign import ccall bin ::
     CInt         -- # of points
  -> Double       -- scale
  -> Double       -- wstep
  -> Ptr CxDouble -- Vis ptr
  -> Ptr UVW      -- UVWs ptr
  -> Ptr CInt     -- GCF supports ptr
  -> CInt         -- grid size
  -> IO (HostPtr ())

fromVec :: Vector a -> Ptr a
fromVec (CVector _ p) = castPtr p
fromVec _ = error "Wrong CPU vector location."

prepare :: GridPar -> Vis -> GCFSet -> IO (Vis, GCFSet)
prepare gridp vis gcfSet = withArray gcfSupps $ \sp -> do
    binned <- bin
                (fi numOfPoints)
                scale
                wstep
                (fromVec $ visData vis)
                (fromVec $ visPositions vis)
                (advancePtr sp maxWPlane)
                (fi grSize)
    sizeInBytes <- peek (castPtr $ useHostPtr binned) :: IO CInt
    return (vis{visBinData = HostVector (fromIntegral sizeInBytes) binned}, gcfSet)
  where
    fi = fromIntegral
    numOfPoints = visTimesteps vis * length (visBaselines vis)
    gcfp = gcfsPar gcfSet
    grSize = gridWidth gridp
    scale = fromIntegral grSize / gridLambda gridp
    wstep = gcfpStepW gcfp
    size i = min (gcfpMaxSize gcfp) (gcfpMinSize gcfp + gcfpGrowth gcfp * abs i)
    maxWPlane = let (DeviceVector wplanes _) = gcfTable gcfSet in wplanes `div` 2
    gcfSupps = map (fi . size) [-maxWPlane .. maxWPlane]

createGrid :: GridPar -> GCFPar -> IO UVGrid
createGrid gp _ = do
   dat@(DeviceVector _ p) <- allocDeviceVector (gridFullSize gp)
   memset p (fromIntegral $ vectorByteSize dat) 0
   return $ UVGrid gp 0 dat

-- foreign import ccall unsafe "&" gridKernelGatherFullGCF :: Fun
foreign import ccall unsafe "&" gridKernelGatherHalfGCF :: Fun

grid :: Vis -> GCFSet -> UVGrid -> IO UVGrid
grid vis gcfSet uvgrid = do
    dataOff <- peek (plusPtr p ciSiz) :: IO CInt
    let
      dataOffi = fromIntegral dataOff
      dataSiz = siz - dataOffi
      dataVec :: Vector Word8
      dataVec = HostVector dataSiz (HostPtr $ plusPtr p dataOffi)
      visOffsPtr = plusPtr p (ciSiz * 2)
      preOffsPtr = plusPtr visOffsPtr offsetsBlockLen
      nOfItemsPtr = plusPtr preOffsPtr offsetsBlockLen
      peekVisOff = peekOffVal visOffsPtr
      peekPreOff = peekOffVal preOffsPtr
      peekNOfItems = peekOffVal nOfItemsPtr
    -- In principle after marshalling we need no
    -- Vis and UVW parts of the data anymore, but
    -- we still need offsets and number-of-values data
    -- which share the same underlying buffer, hence
    -- we keep the whole thing in memory until the gridding finishes.
    devData@(DeviceVector _ dp) <- dupDeviceVector dataVec
    let
      processBin up vp = do
        n <- peekNOfItems up vp
        if n > 0
          then do
                 viso <- peekVisOff up vp
                 preo <- peekPreOff up vp
                 -- launchKernel gridKernelGatherFullGCF (grLin, grLin, 1) (trLin, trLin, 1) 0 Nothing
                 --   $ mapArgs (plusDevPtr dp preo) (plusDevPtr dp viso) (advanceDevPtr gcftp maxWPlane) (uvgData uvgrid) n up vp grSize bstep
                 launchKernel gridKernelGatherHalfGCF (grLin, grLin, 1) (trLin, trLin, 1) 0 Nothing
                   $ mapArgs (plusDevPtr dp preo) (plusDevPtr dp viso) gcftp (uvgData uvgrid) n up vp grSize bstep
          else return ()
    sequence_ [processBin up vp | up <- [0 .. numOfDivs - 1], vp <- [0 .. numOfDivs - 1]]
    sync
    freeVector h
    freeVector devData
    return uvgrid
  where
    numOfDivs = 32
    offsetsBlockLen = numOfDivs * numOfDivs * ciSiz
    peekOffVal :: Ptr CInt -> Int -> Int -> IO Int
    peekOffVal tab bu bv = fmap fromIntegral $ peek (advancePtr tab $ bu * offsetsBlockLen + bv)
    h@(HostVector siz (HostPtr p)) = visBinData vis
    ciSiz = sizeOf (undefined :: CInt)
    grSize = gridWidth (uvgPar uvgrid)
    bstep = grSize `div` numOfDivs
    trLin = 16 -- 16x16 threads in block
    grLin = bstep `div` trLin
    DeviceVector _wplanes gcftp = gcfTable gcfSet
    -- maxWPlane = _wplanes `div` 2

degrid :: UVGrid -> GCFSet -> Vis -> IO Vis
degrid = Nvidia.degrid
