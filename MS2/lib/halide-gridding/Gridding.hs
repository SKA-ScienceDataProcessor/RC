-- |Gridding.hs
--
-- Implementation of gridding algorithm in Halide, interface from Haskell code.
--
-- Copyright (C) 2014 Braam Research, LLC.

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Gridding
        ( GriddingMode(..)
        , GriddingInput
        ) where

import Control.Exception

import Foreign.Ptr

import System.IO

-- |Opaque data type to type pointers to Halide images.
data HalideImage a

-- |The pointer itself.
type HalideImagePtr a = Ptr (HalideImage a)


data GriddingMode = GriddingSimple | GriddingInterpolate | GriddingOversample | GriddingCompute
        deriving (Eq, Ord, Show, Enum)

-- |Gridding input. Opaque to the user.
data GriddingInput = GriddingInput {
          griddingInputMode             :: GriddingMode -- defines presence and structure of supports.
        , griddingInputUVW              :: HalideImage Float
        , griddingInputVisibilities     :: HalideImage Float
        , griddingSupports              :: Maybe (HalideImage Float)
        }

-- |Allocating 1D image.
foreign import ccall "halideFloatImage1D" halideFloatImage1D :: Int -> IO (HalideImagePtr Float)

-- |Allocating 2D image.
foreign import ccall "halideFloatImage2D" halideFloatImage2D :: Int -> Int -> IO (HalideImagePtr Float)

-- |Allocating 3D image.
foreign import ccall "halideFloatImage3D" halideFloatImage3D :: Int -> Int -> Int -> IO (HalideImagePtr Float)

-- |Allocating 4D image.
foreign import ccall "halideFloatImage4D" halideFloatImage4D :: Int -> Int -> Int -> Int -> IO (HalideImagePtr Float)


-- |Read input parameters from file.
-- During this we allocate all necessary Halide images.
readGriddingData :: GriddingMode -> FilePath -> IO GriddingInput
readGriddingData mode filePath = do
        bracket (openBinaryFile filePath ReadMode) hClose $ \h -> do
                error "readGriddingData is not yet done!"
