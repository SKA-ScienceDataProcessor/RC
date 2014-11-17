-- |DNA.Gridding.hs
--
-- Implementation of gridding algorithm in Halide, interface from Haskell code.
--
-- Copyright (C) 2014 Braam Research, LLC.

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module DNA.Gridding
        ( GriddingMode(..)
        , GriddingInput
        , readGriddingData
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

-- Importing internal structures.
import DNA.Gridding.Internal

data GriddingMode = GriddingSimple | GriddingInterpolate | GriddingOversample | GriddingCompute
        deriving (Eq, Ord, Show, Enum)

-- |Gridding input. Opaque to the user.
data GriddingInput = GriddingInput {
          griddingInputMode             :: GriddingMode -- defines presence and structure of supports.
        , griddingInputUVW              :: HalideFloatImagePtr
        , griddingInputVisibilities     :: HalideFloatImagePtr
        , griddingSupports              :: Maybe HalideFloatImagePtr
        }

-- |Read Int64 from file.
readInt64 :: Fd -> IO Int64
readInt64 fd = do
        bytes <- PBS.fdRead fd 8
        return $ runGet get bytes

-- |Seeking the beginning of 4096 page.
seekPageBegin :: Int64 -> Fd -> IO ()
seekPageBegin chunksPages fd = do
        let     page = chunksPages + 1
        fdSeek fd AbsoluteSeek (fromIntegral page * 4096)
        return ()

readImageContent :: Int64 -> Fd -> HalideFloatImagePtr -> IO Int64
readImageContent pagesCount fd@(Fd cfd) image = do
        sizeExpected <- halideFloatImageDataSize image
        sizeRead <- readHalideFloatImage image cfd
        when (sizeRead < sizeExpected) $ error $ "error reading image, too less data read or I/O error."
        return sizeRead

-- |Read input parameters from file.
-- During this we allocate all necessary Halide images.
readGriddingData :: GriddingMode -> FilePath -> IO (Maybe GriddingInput)
readGriddingData mode filePath = flip catchIOError (const $ return Nothing) $ do
        h <- openFd filePath ReadWrite Nothing defaultFileFlags
        flip catchIOError (const $ closeFd h >> return Nothing) $ do
                baselines <- readInt64 h
                timesteps <- readInt64 h
                uvwImage <- halideFloatImage3D (fromIntegral baselines) (fromIntegral timesteps) 3
                visibilities <- halideFloatImage3D (fromIntegral baselines) (fromIntegral timesteps) 8
                pagesRead <- readImageContent 0 h uvwImage
                readImageContent pagesRead h visibilities
                return $ Just $ GriddingInput {
                          griddingInputMode = mode
                        , griddingInputUVW = uvwImage
                        , griddingInputVisibilities = visibilities
                        , griddingSupports = Nothing
                        }
