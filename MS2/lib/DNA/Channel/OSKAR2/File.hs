-- |DNA.Channel.OSKAR2.File.hs
--
-- Reads OSKAR2 Binary files.
--
-- Copyright (C) 2014 Braam Research, LLC

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, BangPatterns #-}

module DNA.Channel.OSKAR2.File (
          readOSKAR2BinFileUVWVisibilities
        ) where

import qualified Control.DeepSeq as CD

import Control.Distributed.Process
import qualified Control.Distributed.Process.Platform.UnsafePrimitives as Unsafe

import Control.Monad

import Data.Binary

import GHC.Generics (Generic)

import Data.Typeable

import Data.Int (Int64)
import Data.Vector.Binary ()

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

readOSKAR2BinFileUVWVisibilities :: FilePath -> HalideFloatImagePtr -> IO ()
readOSKAR2BinFileUVWVisibilities fn image = error "not implemented!!"