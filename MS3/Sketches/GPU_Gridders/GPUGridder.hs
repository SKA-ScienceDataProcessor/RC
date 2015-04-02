{-# LANGUAGE
      CPP
    , ScopedTypeVariables
    , FlexibleInstances
    , TypeSynonymInstances
    , DeriveGeneric
  #-}

module GPUGridder where

-- import Foreign.Storable
import Foreign.Storable.Complex ()
import qualified CUDAEx as CUDA

import GHC.Generics (Generic)
import Data.Binary
import BinaryInstances ()

import ArgMapper
import OskarBinReaderFFI

#ifndef QUICK_TEST
import Paths_dna_ms3 ( getDataFileName )
#else
#define getDataFileName return
#endif

data Grid = Grid {
    gridSize :: !Int
  , gridPtr  :: !(CUDA.DevicePtr CUDA.CxDoubleDevPtr)
  } deriving (Generic)

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
launchAddBaselines ::
      CUDA.Fun
   -> Double
   -> Int
   -> Int
   -> CUDA.DevicePtr BlWMap
   -> CUDA.CxDoubleDevPtr
   -> CUDA.DevicePtr CUDA.CxDoubleDevPtr
   -> CUDA.DoubleDevPtr
   -> CUDA.CxDoubleDevPtr
   -> IO ()
launchAddBaselines f scale num_of_baselines max_supp permutations gridptr gcflp uvwp visp =
  CUDA.launchKernel f (num_of_baselines, 1, 1) (nOfThreads, 1, 1) 0 Nothing params
  where
    nOfThreads = min 1024 (max_supp * max_supp)
    params = mapArgs $ scale :. permutations :. gridptr :. gcflp :. uvwp :. visp :. Z
