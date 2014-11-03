-- |Gridding.hs
--
-- Implementation of gridding algorithm in Halide, interface from Haskell code.
--
-- Copyright (C) 2014 Braam Research, LLC.

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Gridding
        ( GriddingMode(..)
        , GriddingInput
        , readGriddingData
        ) where

import Data.Binary
import Data.Binary.Get

import qualified Data.ByteString.Lazy as BL

import Data.Int

import Foreign.Ptr

import System.IO
import System.IO.Error

-- |Opaque data type to type pointers to Halide images.
data HalideImage a

-- |The pointer itself.
type HalideImagePtr a = Ptr (HalideImage a)


data GriddingMode = GriddingSimple | GriddingInterpolate | GriddingOversample | GriddingCompute
        deriving (Eq, Ord, Show, Enum)

-- |Gridding input. Opaque to the user.
data GriddingInput = GriddingInput {
          griddingInputMode             :: GriddingMode -- defines presence and structure of supports.
        , griddingInputUVW              :: HalideImagePtr Float
        , griddingInputVisibilities     :: HalideImagePtr Float
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

-- |Get the size of Halide image, in bytes.
foreign import ccall "halideFloatImageDataSize" halideFloatImageDataSize :: HalideImagePtr Float -> IO Int64

-- |Read Int64 from file.
readInt64 :: Handle -> IO Int64
readInt64 h = do
        bytes <- BL.hGet h 8
        return $ runGet get bytes

-- |Seeking the beginning of 4096 page.
seekPageBegin :: [Int64] -> Handle -> IO ()
seekPageBegin chunksSizes handle = do
        let     toPage x = div (x + 4095) 4096
        let     chunksPages = map toPage chunksSizes
        let     page = sum chunksPages + 1
        hSeek handle AbsoluteSeek (fromIntegral page * 4096)

readImageContent :: [Int64] -> Handle -> HalideImagePtr Float -> IO [Int64]
readImageContent sizes handle image = do
        error "read image!!!"


-- |Read input parameters from file.
-- During this we allocate all necessary Halide images.
readGriddingData :: GriddingMode -> FilePath -> IO (Maybe GriddingInput)
readGriddingData mode filePath = flip catchIOError (const $ return Nothing) $ do
        h <- openBinaryFile filePath ReadMode
        flip catchIOError (const $ hClose h >> return Nothing) $ do
                baselines <- readInt64 h
                timesteps <- readInt64 h
                uvwImage <- halideFloatImage3D (fromIntegral baselines) (fromIntegral timesteps) 3
                visibilities <- halideFloatImage3D (fromIntegral baselines) (fromIntegral timesteps) 8
                sizes <- readImageContent [] h uvwImage
                readImageContent sizes h visibilities
                return $ Just $ GriddingInput {
                          griddingInputMode = mode
                        , griddingInputUVW = uvwImage
                        , griddingInputVisibilities = visibilities
                        , griddingSupports = Nothing
                        }
