{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Foreign.Ptr
import Foreign.Storable

import DNA.Logging

foreign import ccall unsafe _aligned_malloc :: Int -> Int -> IO (Ptr a)

alignedMallocArray :: forall a. Storable a => Int -> Int -> IO (Ptr a)
alignedMallocArray size = _aligned_malloc (size * sizeOf (undefined :: a))

printImmediate :: String -> IO ()
printImmediate = message 0
