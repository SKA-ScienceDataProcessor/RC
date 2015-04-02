{-# LANGUAGE
      CPP
    , ScopedTypeVariables
    , FlexibleInstances
    , TypeSynonymInstances
    , DeriveGeneric
  #-}

module GPUGridder where

import Foreign
import Foreign.C
import Foreign.Storable.Complex ()
import qualified CUDAEx as CUDA

import GHC.Generics (Generic)
import Data.Binary
import BinaryInstances ()

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
  } deriving (Generic)

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
      Int
   -> Int
   -> CUDA.DevicePtr BlWMap
   -> IO ()

type AddBaselinesFun =
      CDouble
   -> CUDA.CxDoubleDevPtr
   -> CUDA.DevicePtr CUDA.CxDoubleDevPtr
   -> CUDA.DoubleDevPtr
   -> CUDA.CxDoubleDevPtr
   -> At

type AddBaselinesIter = Int -> Ptr BlWMap -> At -> IO ()

launchAddBaselines :: CUDA.Fun -> AddBaselinesFun
launchAddBaselines f scale gridptr gcflp uvwp visp numOfBaselines maxSupp permutations =
  CUDA.launchKernel f (numOfBaselines, 1, 1) (nOfThreads, 1, 1) 0 Nothing params
  where
    nOfThreads = min 1024 (maxSupp * maxSupp)
    params = mapArgs $ scale :. permutations :. gridptr :. gcflp :. uvwp :. visp :. Z

data GridConfig = GridConfig {
    gcKernelName :: !String
  , gcGCFIsFull :: !Bool
  , gcIter :: !AddBaselinesIter
  }

runGridder :: GridConfig -> TaskData -> GCF -> IO Grid
runGridder (GridConfig gfname gcfIsFull iter) td gcf = do
    fun <- (`CUDA.getFun` gfname) =<< CUDA.loadFile =<< getDataFileName "scatter_gridders_smem_ska.cubin"
    gridptr <- CUDA.mallocArray gridsize
    CUDA.memset gridptr (fromIntegral $ gridsize * cxdSize) 0
    CUDA.allocaArray uvwSize $ \uvwp ->
      CUDA.allocaArray visSize $ \visp ->
        CUDA.allocaArray nBaselines $ \permp -> do
          CUDA.pokeArray uvwSize (castPtr $ tdUVWs td) uvwp
          CUDA.pokeArray visSize (castPtr $ tdVisibilies td) visp
          CUDA.pokeArray nBaselines perms permp
          iter nBaselines perms (launchAddBaselines fun scale visp gcfptr uvwp visp)
    return $ Grid gridsize gridptr
  where
    scale = (2048 - 124 - 1) / (tdMaxx td) -- 124 max hsupp
    nBaselines = tdBaselines td
    nPoints = tdPoints td
    uvwSize = nPoints * 3
    visSize = nPoints * 4
    perms = tdMap td
    gcfptr = if gcfIsFull then getCentreOfFullGCF gcf else gcfLayers gcf
    gridsize = 4096 * 4096 * 4
    cxdSize = sizeOf (undefined :: CxDouble)
    -- permutations gridptr uvwp visp
