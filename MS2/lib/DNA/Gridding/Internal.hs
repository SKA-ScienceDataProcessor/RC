-- |DNA.Gridding.Internal.hs
--
-- Internal structures of gridding, shared between modules.
--
-- Copyright (C) 2014 Braam Research, LLC.

{-# LANGUAGE EmptyDataDecls #-}

module DNA.Gridding.Internal
        ( HalideImage
        , HalideFloatImagePtr

        ) where

import Control.Monad

import Data.Binary
import Data.Binary.Get

import qualified Data.ByteString.Lazy as BL

import Data.Int

import Foreign.C.Types
import Foreign.Ptr

import System.IO
import System.IO.Error

import System.Posix     -- unix package.
import qualified System.Posix.IO.ByteString.Lazy as PBS -- unix-bytestring package.

-- |Opaque data type to type pointers to Halide images.
data HalideImage

-- |The pointer itself.
type HalideFloatImagePtr = Ptr HalideImage


-- |Allocating 1D image.
foreign import ccall "halideFloatImage1D" halideFloatImage1D :: Int -> IO HalideFloatImagePtr

-- |Allocating 2D image.
foreign import ccall "halideFloatImage2D" halideFloatImage2D :: Int -> Int -> IO HalideFloatImagePtr

-- |Allocating 3D image.
foreign import ccall "halideFloatImage3D" halideFloatImage3D :: Int -> Int -> Int -> IO HalideFloatImagePtr

-- |Allocating 4D image.
foreign import ccall "halideFloatImage4D" halideFloatImage4D :: Int -> Int -> Int -> Int -> IO HalideFloatImagePtr

-- |Get the size of Halide image, in bytes.
foreign import ccall "halideFloatImageDataSize" halideFloatImageDataSize :: HalideFloatImagePtr -> IO Int64

-- |Read data into Halide image. Returns count of bytes read, negative value means error.
foreign import ccall "readHalideFloatImage" readHalideFloatImage :: HalideFloatImagePtr -> CInt -> IO Int64
