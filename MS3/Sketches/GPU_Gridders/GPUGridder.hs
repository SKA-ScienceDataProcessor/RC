{-# LANGUAGE
      CPP
    , ScopedTypeVariables
    , FlexibleInstances
    , TypeSynonymInstances
    , DeriveGeneric
    , DeriveDataTypeable
  #-}

module GPUGridder where

import Foreign
import Foreign.C
import Foreign.Storable.Complex ()
import qualified CUDAEx as CUDA

import GHC.Generics (Generic)
import Data.Binary
import BinaryInstances ()
import Data.Typeable

import Data.Maybe (catMaybes)
import System.IO.MMap
import Text.Printf (printf)
import System.Directory (doesFileExist)


import ArgMapper
import OskarBinReader
import OskarBinReaderFFI
import GCF

#ifndef QUICK_TEST
import Paths_dna_ms3 ( getDataFileName )
#else
#define getDataFileName return
#endif

data Grid = Grid {
    gridSize :: !Int
  , gridPtr  :: !CUDA.CxDoubleDevPtr
  } deriving (Generic, Typeable)

finalizeGrid :: Grid -> IO ()
finalizeGrid (Grid _ p) = CUDA.free p

instance Binary Grid

{-
 We choose the most flexible scheme here.
 Romein launches his kernel with:
   num_of_baselines = total number of baselines
   max_supp = global maximum support

 But since we have permutations vector which follows baselines sorting
 we can launch our kernels in batches with equal max_supp
 thus greatly increasing overall occupation.
 -}

type At =
      Int                   -- Support size for launch
   -> Int                   -- Baseline offset
   -> Int                   -- Num of baselines in launch
   -> IO ()

type AddBaselinesFun =
      CDouble                             -- Scale
   -> CUDA.CxDoubleDevPtr                 -- Grid ptr
   -> CUDA.DevicePtr CUDA.CxDoubleDevPtr  -- GCF layers ptr
   -> CUDA.DoubleDevPtr                   -- UVW ptr
   -> CUDA.CxDoubleDevPtr                 -- Visibilities ptr
   -> CUDA.DevicePtr BlWMap               -- Device pointer to mapping vector
   -> At

type AddBaselinesIter =
      Int                   -- Num of baselines total
   -> Ptr BlWMap            -- Pointer to mapping vector
   -> At
   -> IO ()

launchAddBaselines :: TaskData -> CUDA.Fun -> AddBaselinesFun
launchAddBaselines td f scale gridptr gcflp uvwp visp permp maxSupp blOff numOfBaselines =
  CUDA.launchKernel f (numOfBaselines, 1, 1) (nOfThreads, 1, 1) 0 Nothing params
  where
    off = blOff * tdTimes td * tdChannels td
    nOfThreads = min 1024 (maxSupp * maxSupp)
    params = mapArgs $  scale
                     :. tdWstep td
                     :. CUDA.advanceDevPtr permp blOff
                     :. gridptr
                     :. gcflp
                     :. CUDA.advanceDevPtr uvwp off
                     :. CUDA.advanceDevPtr visp off
                     :. Z

data GridderConfig = GridderConfig {
    gcKernelName :: !String
  , gcGCFIsFull :: !Bool
  , gcIter :: !AddBaselinesIter
  }

gridderModule :: IO CUDA.Module
gridderModule = getDataFileName "scatter_gridders_smem_ska.cubin" >>= CUDA.loadFile

-- FIXME: Add permutations option to config
--   and *generate* name from gcfIsFull and permutation option
runGridder :: GridderConfig -> TaskData -> GCFDev -> IO Grid
runGridder (GridderConfig gfname gcfIsFull iter) td gcf = do
    fun <- (`CUDA.getFun` gfname) =<< gridderModule
    gridptr <- CUDA.mallocArray gridsize
    CUDA.memset gridptr (fromIntegral $ gridsize * cxdSize) 0
    CUDA.allocaArray uvwSize $ \uvwp ->
      CUDA.allocaArray visSize $ \visp ->
        CUDA.allocaArray nBaselines $ \permp -> do
          CUDA.pokeArray uvwSize (castPtr $ tdUVWs td) uvwp
          CUDA.pokeArray visSize (castPtr $ tdVisibilies td) visp
          CUDA.pokeArray nBaselines perms permp
          iter nBaselines perms (launchAddBaselines td fun scale gridptr gcfptr uvwp visp permp)
    return $ Grid gridsize gridptr
  where
    -- FIXME: Move this to top level and
    --  use this single definition everywhere
    scale = (2048 - 124 - 1) / (tdMaxx td) -- 124 max hsupp
    nBaselines = tdBaselines td
    nPoints = tdPoints td
    uvwSize = nPoints * 3
    visSize = nPoints * 4
    perms = tdMap td
    -- FIXME: and gcfIsFull to GCF itself and move this logic to GCF code
    gcfptr = if gcfIsFull then getCentreOfFullGCF gcf else gcfLayers gcf
    -- FIXME:
    gridsize = 4096 * 4096 * 4
    cxdSize = sizeOf (undefined :: CxDouble)

normalizeAndExtractPolarization :: Int32 -> CUDA.CxDoubleDevPtr -> Grid -> IO ()
normalizeAndExtractPolarization pol polp (Grid _ gridp) = do
  f <- (`CUDA.getFun` "normalizeAndExtractPolarization") =<< gridderModule
  -- 128 * 32 = 4096
  CUDA.launchKernel f (128, 128, 1) (32, 32, 1) 0 Nothing $ mapArgs $ pol :. polp :. gridp :. Z

gatherGridderModule :: IO CUDA.Module
gatherGridderModule = getDataFileName "gather_gridder.cubin" >>= CUDA.loadFile

type RawPtr = CUDA.DevicePtr Word8

-- FIXME: Add permutations option to config
--   and *generate* name from gcfIsFull and permutation option
runGatherGridder :: GridderConfig -> String -> TaskData -> GCFDev -> IO Grid
runGatherGridder (GridderConfig gfname gcfIsFull _) prefix td gcf = do
    fun <- (`CUDA.getFun` gfname) =<< gatherGridderModule
    gridptr <- CUDA.mallocArray gridsize
    CUDA.memset gridptr (fromIntegral $ gridsize * cxdSize) 0
    --
    let processBin (c, up, vp) = do
          let
            binbasename = printf "%s%06d-%03d-%03d" prefix c up vp
            binprename = binbasename ++ "-pre.dat"
            binvisname = binbasename ++ "-vis.dat"
          doMe <- doesFileExist binprename
          if doMe
            then do
              (pre_data_ptr_host, pre_data_rawsize, pre_data_offset, pre_data_size) <- mmapFilePtr binprename ReadOnly Nothing
              pre_data_in <- CUDA.mallocArray pre_data_size :: IO RawPtr
              CUDA.pokeArray pre_data_size (plusPtr pre_data_ptr_host pre_data_offset) pre_data_in
              --
              (vis_data_ptr_host, vis_data_rawsize, vis_data_offset, vis_data_size) <- mmapFilePtr binvisname ReadOnly Nothing
              vis_data_in <- CUDA.mallocArray vis_data_size :: IO RawPtr
              CUDA.pokeArray vis_data_size (plusPtr vis_data_ptr_host vis_data_offset) vis_data_in

              let len = fromIntegral vis_data_size `div` vis_size
              CUDA.launchKernel fun (16,16, 1) (8,8,1) 0 Nothing
                $ mapArgs $ up :. vp :. pre_data_in :. vis_data_in :. len :. gcfptr :. gridptr :. Z
              --
              munmapFilePtr pre_data_ptr_host pre_data_rawsize
              munmapFilePtr vis_data_ptr_host vis_data_rawsize
              --
              return $ Just (pre_data_in, vis_data_in)
            else return Nothing
    resourcesToFree <- mapM processBin [(c, up, vp) | c <- [0::Int .. tdChannels td-1], up <- [0::Int32 .. 31], vp <- [0::Int32 .. 31]]
    -- Cleanup
    mapM_ (\(p, v) -> CUDA.free p >> CUDA.free v) (catMaybes resourcesToFree)
    return $ Grid gridsize gridptr
  where
    -- FIXME: and gcfIsFull to GCF itself and move this logic to GCF code
    gcfptr = if gcfIsFull then getCentreOfFullGCF gcf else gcfLayers gcf
    -- FIXME:
    gridsize = 4096 * 4096 * 4
    cxdSize = sizeOf (undefined :: CxDouble)
    vis_size = 64 :: Int32
